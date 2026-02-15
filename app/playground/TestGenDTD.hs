import BiFlux.DTD.GenHaskellDTD

genbooks = genHaskellDTD "Test/test/bookstore/books.dtd" "Test/test/bookstore/Books.hs"
genbookstore = genHaskellDTD "Test/test/bookstore/bookstore.dtd" "Test/test/bookstore/Bookstore.hs"

gennetscape = genHaskellDTD "Test/test/bookmark/netscape.dtd" "Test/test/bookmark/Netscape.hs"
genxbel = genHaskellDTD "Test/test/bookmark/xbel.dtd" "Test/test/bookmark/Xbel.hs"
