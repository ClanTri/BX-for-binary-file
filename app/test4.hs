import Generics.BiGUL
import qualified Generics.BiGUL.Interpreter as B

intSync :: BiGUL Int Int
intSync = Replace

main :: IO ()
main = do
    let source = 100      -- 假设这个是 "bin" 中的值
    let view   = 42       -- 假设这个是 "bson" 中的值

    let result = B.put intSync view source

    case result of
      Right synced -> putStrLn $ "Synchronized value: " ++ show synced
      Left err     -> putStrLn $ "Error: " ++ err
