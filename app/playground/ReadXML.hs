module ReadXML where

import BiFlux.DTD.TypeDef
import qualified Books
import BiFlux.DTD.GenHaskellDTD
import Text.XML.HaXml.DtdToHaskell.TypeDef
import qualified Bookstore

--loadBooksXML = loadXml "Test/test/bookstore/books.xml" :: IO Books.Books

--testWriteXML = do
--  books <- loadBooksXML
--  writeXml "Test/test/bookstore/test.xml" books
--


tPPDTG0 = ppDataTypeGeneric "Books" (DataDef False (Name "books" "books") [] [])

tPPDTG1 = ppDataTypeGeneric "Books" (DataDef False (Name "books" "Books") [] [(Name "books" "Books", [String, String])])
