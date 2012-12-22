import Control.Concurrent.Async
import Control.Monad
import Data.List
import System.Environment
import Text.HandsomeSoup
import Text.XML.HXT.Core

extractGalleryPage :: String -> IO [String]
extractGalleryPage url = do
  doc <- fromUrl url
  images <- runX $ doc >>> css "#gallery a" ! "href"
  movies <- runX $ doc >>> css "source" ! "src"
  return $ images ++ movies

printImageUrls :: String -> IO ()
printImageUrls url = mapM_ putStrLn =<< extractGalleryPage url

main = do
  mapConcurrently printImageUrls =<< getArgs
