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

main = do
  links <- liftM concat $ mapM extractGalleryPage =<< getArgs
  mapM_ putStrLn $ links
