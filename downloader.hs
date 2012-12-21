import Data.List
import Text.HandsomeSoup
import Text.XML.HXT.Core

extractGalleryPage :: String -> IO [String]
extractGalleryPage url = do
  doc <- fromUrl url
  images <- runX $ doc >>> css "#gallery a" ! "href"
  movies <- runX $ doc >>> css "source" ! "src"
  return $ images ++ movies

main = do
  links <- extractGalleryPage "URL"
  mapM_ (putStrLn . show) $ links
