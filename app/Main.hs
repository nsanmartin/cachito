{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Control.Concurrent
import Data.Binary.Builder (fromByteString, append, singleton)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Internal (c2w)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock
import Network.HTTP.Simple
import Network.HTTP.Types (status200)
import Network.Wai (rawPathInfo, responseBuilder)
import Network.Wai.Handler.Warp (run)
import System.Directory 
import System.Environment (getArgs)
import System.FilePath
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Search (replace)

expirationTimeSecs = 60 * 60 * 24

replaceAnchors s = B8.toStrict 
                 $ replace (B8.pack "href=\"https://") (B8.pack "href=\"http://localhost:3000/www$" ) s

replaceRelativeAnchors baseUrl s
    = B8.toStrict
    $ replace (B8.pack "href=\"/") (B8.append (B8.pack "href=\"http://localhost:3000/www$") relativeUrl) s
    where relativeUrl = (B8.dropWhileEnd (/= '/')) $ (B8.dropWhileEnd (== '/')) baseUrl


fetchAndStoreInto :: B8.ByteString -> FilePath -> IO B8.ByteString
fetchAndStoreInto url fname = do
    parsedUrl <- parseRequest $ B8.unpack $ (proto <>) url
    req <- httpBS parsedUrl
    let body = replaceRelativeAnchors url $ replaceAnchors $ getResponseBody req
    B8.writeFile  fname body
    return body

createDirForFile :: FilePath -> IO ()
createDirForFile fname = createDirectoryIfMissing True 
                       $ takeDirectory fname

hasTimeExpiredForFile :: FilePath -> IO Bool
hasTimeExpiredForFile fname =  do
    time <- getCurrentTime
    modTime <- getModificationTime fname
    let myNominalDiffTime = diffUTCTime time modTime
    let (diffTimeInSeconds, _) = properFraction myNominalDiffTime
    return $ diffTimeInSeconds < expirationTimeSecs


urlToPath :: B8.ByteString -> FilePath 
urlToPath = B8.unpack . (proto <>) . (<> extension)

fetchUrl :: B8.ByteString -> IO B8.ByteString
fetchUrl url = do
    let fname = urlToPath $ B8.dropWhileEnd (== '/') url
    fileIsCached <- doesFileExist fname >>= \case 
        True -> hasTimeExpiredForFile fname
        False -> return False
    if fileIsCached
    then B8.readFile fname
    else createDirForFile fname *> fetchAndStoreInto url fname


endl :: BSB.Builder
endl = singleton $ c2w '\n'

proto = B8.pack "https://"
extension = B8.pack ".html"

www url respond = do
    fetchedUrl <- fetchUrl url
    respond $
        responseBuilder
        status200
        [("Content-Type", "text/html")]
        $ fromByteString fetchedUrl 

unknownCmd url respond = 
    respond $
        responseBuilder
        status200
        [("Content-Type", "text/html")]
        $ fromByteString $ B8.append "unknownCmd: " url


application req respond = do
    let fullUrl = rawPathInfo req :: B8.ByteString
    case (B8.break (== '$') fullUrl) of
        ("/www", url) -> case B8.uncons url of
             Just ('$', tailUrl) -> www tailUrl respond
             _ -> unknownCmd "no url" respond
        (cmd, _) -> unknownCmd cmd respond
        

main = run 3000 application
