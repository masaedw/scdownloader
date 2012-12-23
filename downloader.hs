import Control.Concurrent.Async
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import Text.HandsomeSoup
import Text.XML.HXT.Core

main = do
  mapConcurrently printImageUrlsIfNotDownloaded =<< getArgs

printImageUrlsIfNotDownloaded :: String -> IO ()
printImageUrlsIfNotDownloaded url = do
  urls <- extractGalleryPage url
  mapM_ putStrLn =<< filterM (liftM not . downloaded) urls

downloaded :: String -> IO Bool
downloaded  = doesFileExist . filename

filename :: String -> FilePath
filename = reverse . takeWhile (/= '/') . reverse

extractGalleryPage :: String -> IO [String]
extractGalleryPage url = do
  doc <- fromUrl url
  images <- runX $ doc >>> css "#gallery a" ! "href"
  movies <- runX $ doc >>> css "source" ! "src"
  return $ images ++ movies
