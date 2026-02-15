{-# LANGUAGE ViewPatterns #-}

import Generics.BiGUL.MonadBiGULError
import Generics.BiGUL.AST
import Generics.BiGUL.Interpreter
import Control.Monad
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec hiding ((<+>), Line)
import Text.ParserCombinators.Parsec.Pos hiding (Line)
import System.IO
import System.Process
import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Maybe

type ApiCalls     = [SCall]
type SCall        = (SPermissions, (File, (Line, (Protected, Deleted))))
type File         = String
type Line         = Int
type Deleted      = Bool
type Name         = String
type SPermissions = [SPermission]
type SPermission  = (Name, [ApiVersion])
type Protected    = String
type ApiVersion   = String

type Calls        = [VCall]
type VCall        = (File, (Line, VPermissions))
type VPermissions = [VPermission]
type VPermission  = Name

t :: MonadError' e m => ApiVersion -> BiGUL m ApiCalls Calls
t ver = Align (\(ps, (f, (l, (ptd, del)))) -> return (not del))
              (\(_, (f, (l, _))) (f', (l', _)) -> return (f == f' && l == l'))
              (Rearr (RVar `RProd` (RVar `RProd` RVar))
                       (EDir (DRight (DRight DVar)) `EProd`
                       (EDir (DLeft DVar) `EProd`
                       (EDir (DRight (DLeft DVar)) `EProd`
                       (EConst () `EProd` EConst ()))))
                     (Update (UVar (Align (\(_, vers) -> return (ver `elem` vers))
                                          (\_ _ -> return True)
                                          (Rearr RVar (EDir DVar `EProd` EConst ())
                                                 (Update (UVar Replace `UProd` UVar Skip)))
                                          (\_ -> return ("", [ver]))
                                          (\_ -> return Nothing)) `UProd`
                             (UVar Replace `UProd`
                             (UVar Replace `UProd`
                             (UVar Skip `UProd` UVar Skip))))))
              (\_ -> return ([], ("", (0, ("", False)))))
              (\(ps, (f, (l, (ptd, del)))) -> return (Just (ps, (f, (l, (ptd, True))))))

s :: ApiCalls
s = [([("android.permission.INTERNET", ["4.0", "4.1", "4.1.1"])],
         ("./smali/b.java", (24, ("java/net/URL;->openConnection", False)))),
     ([("android.permission.INTERNET", ["4.1", "4.1.1"])],
         ("./smali/b.java", (576, ("java/net/HttpURLConnection;->connect", False)))),
     ([("android.permission.INTERNET", ["4.0"])],
         ("./smali/c.java", (9, ("", False)))),
     ([("android.permission.INTERNET", ["4.0", "4.1", "4.1.1"]),
       ("android.permission.WAKE_LOCK", ["4.0", "4.1", "4.1.1"])],
         ("./smali/com/appmk/book/main/FlipBookActivity.java", (373, ("java/net/URL;->openConnection", False))))]

v :: Calls
v = [("./smali/b.java",(24,["android.permission.INTERNET"])),
     ("./smali/b.java",(576,["android.permission.INTERNET"])),
     ("./smali/c.java",(9,[])),
     ("./smali/com/appmk/book/main/FlipBookActivity.java",(373,["android.permission.INTERNET","android.permission.WAKE_LOCK"]))]

v' :: Calls
v' = [("./smali/b.java",(24,["android.permission.INTERNET"])),
      ("./smali/b.java",(576,["android.permission.INTERNET"])),
      ("./smali/com/appmk/book/main/FlipBookActivity.java",(373,["android.permission.INTERNET","android.permission.WAKE_LOCK"])),
      ("./smali/d.java", (45, ["android.permission.WIFI"]))]



--type ApiCalls     = [SCall]
--type SCall        = (SPermissions, (File, (Line, (Protected, Deleted))))
--type File         = String
--type Line         = Int
--type Deleted      = Bool
--type Name         = String
--type SPermissions = [SPermission]
--type SPermission  = (Name, [ApiVersion])
--type Protected    = String
--type ApiVersion   = String
--
--type Calls        = [VCall]
--type VCall        = (File, (Line, VPermissions))
--type VPermissions = [VPermission]
--type VPermission  = Name

pXML :: ApiCalls -> String
pXML = element "apicalls" . concat . map (\(ps, (f, (l, (ptd, del)))) -> element "call" (element "permissions" (concat (map (\(n, vers) -> element "permission" (element "name" n ++ concat (map (element "apiversion") vers))) ps)) ++ element "file" f ++ element "line" (show l) ++ element "protected" ptd ++ element "deleted" (map toLower (show del))))

element :: String -> String -> String
element e = (("<" ++ e ++ ">") ++) . (++ ("</" ++ e ++ ">"))

pVXML :: Calls -> String
pVXML = element "calls" . concat . map (\(f,(l, ps)) -> element "call" (element "file" f ++ element "line" (show l) ++ element "permissions" (concat ( map (element "permission" . element "name") ps ))))

lintXML :: String -> IO String
lintXML = readProcess "xmllint" ["--format", "-"]

writeXML :: String -> String -> IO ()
writeXML fname str = lintXML str >>= writeFile fname

(>|>=) :: Monad m => m a -> (a -> m b) -> m a
(>|>=) mx f = mx >>= \x -> f x >> return x

(>|>) :: Monad m => m a -> m b -> m a
(>|>) mx my = mx >|>= const my

alternatives :: [GenParser tok st a] -> GenParser tok st a
alternatives = foldr1 ((<|>) . try)

data XMLToken = Header | Begin String | End String | PCData String deriving Show

xmlTokeniser :: Parser XMLToken
xmlTokeniser =
  alternatives
    [spaces >> string "<?xml" >> many (satisfy (/= '?')) >> string "?>" >> return Header,
     spaces >> char '<' >> spaces >>
       alternatives
         [char '/' >> spaces >>
            (liftM End (many1 (satisfy (\c -> not (isSpace c || c == '>')))) >|>
               (spaces >> char '>')),
          liftM Begin (many1 (satisfy (\c -> not (isSpace c || c == '>')))) >|> (spaces >> char '>')],
     liftM (PCData . despecialise) (many1 (satisfy (/= '<')))]

despecialise :: String -> String
despecialise [] = []
despecialise (stripPrefix "&gt;" -> Just xs) = '>' : despecialise xs
despecialise (x : xs) = x : despecialise xs

header :: GenParser XMLToken () ()
header = token show (const (initialPos "")) (\t -> case t of { Header -> Just (); _ -> Nothing })

beginElement :: String -> GenParser XMLToken () ()
beginElement name = token show (const (initialPos ""))
                      (\t -> case t of { Begin s -> if s == name then Just () else Nothing; _ -> Nothing })

endElement :: String -> GenParser XMLToken () ()
endElement name = token show (const (initialPos ""))
                    (\t -> case t of { End s -> if s == name then Just () else Nothing; _ -> Nothing })

pcdata :: GenParser XMLToken () String
pcdata = token show (const (initialPos "")) (\t -> case t of { PCData s -> Just s; _ -> Nothing })

optPcdata :: GenParser XMLToken () String
optPcdata = liftM (maybe "" (filter (not . isSpace))) (optionMaybe pcdata)

xmlElement :: String -> GenParser XMLToken () a -> GenParser XMLToken () a
xmlElement name p = beginElement name >> p >|> endElement name

apiCallsParser :: GenParser XMLToken () ApiCalls
apiCallsParser = (optional header >>) $
  xmlElement "apicalls" $ many $  xmlElement "call" $ do
    ps <- xmlElement "permissions" $
          many (xmlElement "permission" $ do
            name <- xmlElement "name" optPcdata
            vers <- many (xmlElement "apiversion" optPcdata)
            return (name, vers))
    f <- xmlElement "file" optPcdata
    l <- liftM (\s -> if not (null s) && and (map isDigit s) then read s else 0) $ xmlElement "line" optPcdata
    ptd <- xmlElement "protected" optPcdata
    del <- liftM (\s -> case s of { "true" -> True; "false" -> False }) $ xmlElement "deleted" optPcdata
    return (ps, (f, (l, (ptd, del))))

callsParser :: GenParser XMLToken () Calls
callsParser = (optional header >>) $
  xmlElement "calls" $ many $ xmlElement "call" $ do
    f <- xmlElement "file" optPcdata
    l <- liftM read $ xmlElement "line" optPcdata
    ps <- xmlElement "permissions" (many (xmlElement "permission" (xmlElement "name" optPcdata)))
    return (f, (l, ps))

tokeniseAndParse :: Parser tok -> GenParser tok () a -> ([tok] -> [tok]) -> String -> Either ParseError a
tokeniseAndParse tokeniser parser f = (parse parser "" =<<) . fmap f . parse (many tokeniser >|> eof) ""

expandEmptyElements :: [XMLToken] -> [XMLToken]
expandEmptyElements [] = []
expandEmptyElements (Begin tok@(last -> '/') : toks) =
  let tok' = init tok in Begin tok' : End tok' : expandEmptyElements toks
expandEmptyElements (tok : toks) = tok : expandEmptyElements toks

parseApiCalls :: String -> Either ParseError ApiCalls
parseApiCalls = tokeniseAndParse xmlTokeniser apiCallsParser expandEmptyElements

parseCalls :: String -> Either ParseError Calls
parseCalls = tokeniseAndParse xmlTokeniser callsParser expandEmptyElements








-- command line tool
main = menu

menu :: IO ()
menu = do
  argv <- getArgs
  opts <- parseOptions startOpt options argv
  -- print $ show opts
  if optDirection opts
    then do
      putStrLn "===> Forward transformation:"
      putStrLn "    read source xml"
      srcFilePath <- run "input XML source file missing" $ optInputSourceFile opts
      case optOutputFile opts of
        Nothing -> do
          getCalls srcFilePath "newView.xml" (optApiVersion opts)
        Just optFilePath -> do
          getCalls srcFilePath optFilePath  (optApiVersion opts)
    else do
      putStrLn "==> backward transformation:"
      putStrLn "    read source xml"
      srcFilePath  <- run "input XML source file missing" $ optInputSourceFile opts
      putStrLn "    read view xml"
      viewFilePath <- run "input XML view file missing" $ optInputTargetFile opts
      case optOutputFile opts of
        Nothing ->
          putCalls srcFilePath viewFilePath "newSource.xml" (optApiVersion opts)
        Just optFilePath ->
          putCalls srcFilePath viewFilePath optFilePath  (optApiVersion opts)
  putStrLn "==> Transformation successfully done !"




getCalls :: String -> String -> ApiVersion  -> IO ()
getCalls sourceFileName targetFileName apiVersion = do
  srcXML <- readFile sourceFileName
  either putStrLn
     ((>>= writeFile targetFileName) . lintXML . pVXML) $
           either (Left . show) Right (parseApiCalls srcXML)
           >>= either (Left . show) Right . get (t apiVersion)


putCalls :: String -> String -> String -> ApiVersion -> IO ()
putCalls sourceFileName targetFileName newSourceFileName apiVersion = do
  srcXML <- readFile sourceFileName
  targetXML <- readFile targetFileName
  either putStrLn
     ((>>= writeFile newSourceFileName) . lintXML . pXML) $
     errTrans (parseApiCalls srcXML) >>=
      \apicalls -> errTrans (parseCalls targetXML) >>=
      \calls -> errTrans (put (t apiVersion) apicalls calls)

errTrans :: Show a => Either a b -> Either String b
errTrans = either (Left . show) Right


data Options = Options
  {
    optInputSourceFile   :: Maybe FilePath
  , optInputTargetFile   :: Maybe FilePath
  , optOutputFile        :: Maybe FilePath
  , optDirection         :: Bool
  , optApiVersion        :: String
  } deriving Show

startOpt :: Options
startOpt = Options
  { optInputSourceFile = Nothing
  , optInputTargetFile = Nothing
  , optOutputFile      = Nothing
  , optDirection       = False
  , optApiVersion      = ""
  }

options :: Opts Options
options =
   [ Option "h" ["help"]
        (NoArg (\opt -> exitHelp options))
        "Show usage info"
   , Option "s" ["source"]
        (ReqArg (\arg opt -> return opt { optInputSourceFile = Just arg }) "FILE")
        "Input XML Source File"
   , Option "v" ["view"]
        (ReqArg (\arg opt -> return opt { optInputTargetFile = Just arg }) "FILE")
        "Input XML View File"
   , Option "o" ["output"]
        (ReqArg (\arg opt -> return opt { optOutputFile = Just arg }) "FILE")
        "Output XML File (default: stdout)"
   , Option "a"  ["apiVersion"]
        (ReqArg (\arg opt -> return opt { optApiVersion =  arg }) "FILE")
        "Input ApiVersion"
   , Option "f" ["forward"]
        (NoArg (\opt -> return opt { optDirection = True }))
        "Run the bidirectional transformation in the forward direction (requires an source file)"
   , Option "b" ["backward"]
        (NoArg (\opt -> return opt { optDirection = False }))
        "Run the bidirectional transformation in the backward direction (requires a source file and a modified target file)"
   ]




type Opts a = [OptDescr (a -> IO a)]

parseOptions :: a -> Opts a -> [String] -> IO a
parseOptions start opts argv = case getOpt RequireOrder opts argv of
  (optsActions,rest,[]) -> foldl (>>=) (return start) optsActions
  (_,_,errs) -> do
    prg <- getProgName
    ioError (userError (concat errs ++ usageInfo ("Usage: "++prg++" [OPTION...] files...") opts))

run :: String -> Maybe a -> IO a
run err = maybe (error err) return

-- | Function that prints the help usage and returns with a success code (used
--   when the help program option is specified.
exitHelp :: Opts a -> IO a
exitHelp opts = do
    showHelp opts
    exitWith ExitSuccess

-- | Function that prints the program usage to the sdtderr using the standard
--   'usageInfo' function.
showHelp :: Opts a -> IO ()
showHelp opts = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo ("Usage: "++prg++" [OPTION...]") opts)
    hFlush stderr
