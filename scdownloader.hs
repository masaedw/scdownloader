import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad
import Control.ThreadPool (threadPoolIO)
import Data.List
import System.Directory
import System.Environment
import Text.HandsomeSoup
import Text.XML.HXT.Core

main = do
  galleries <- parseTopPage
  runPool 5 $ map printImageUrlsIfNotDownloaded galleries

parseTopPage :: IO [String]
parseTopPage = do
  doc <- fromUrl "http://www.s-cute.com/"
  runX $ doc >>> css ".newcontent h5 a" ! "href"

printImageUrlsIfNotDownloaded :: String -> IO ()
printImageUrlsIfNotDownloaded url = do
  urls <- extractGalleryPage url
  mapM_ putStrLn =<< filterM (liftM not . downloaded) urls

downloaded :: String -> IO Bool
downloaded = doesFileExist . filename

filename :: String -> FilePath
filename = reverse . takeWhile (/= '/') . reverse

extractGalleryPage :: String -> IO [String]
extractGalleryPage url = do
  doc <- fromUrl url
  images <- runX $ doc >>> css "#gallery a" ! "href"
  movies <- runX $ doc >>> css "source" ! "src"
  return $ images ++ movies

-- http://stackoverflow.com/questions/9193349/how-do-i-create-a-thread-pool
runPool :: Int -> [IO a] -> IO [a]
runPool n as = do
  (input, output) <- threadPoolIO n id
  forM_ as $ writeChan input
  sequence (take (length as) . repeat $ readChan output)
