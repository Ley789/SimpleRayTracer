{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module CmdLine
       (
         mainWith
       , defaultMain
       , multiMain
       , animMain
       , Ray
       , B
       ) where

import           Options.Applicative
import           BackendRayTrace
import           Data.Maybe                      (fromMaybe)
import           Diagrams.Backend.CmdLine
import           Diagrams.Prelude                hiding (height, interval,
                                                  option, output, width, (<>))
import           Data.List.Split


defaultMain :: QDiagram Ray V3 Double Any -> IO ()
defaultMain = mainWith

instance Mainable (QDiagram Ray V3 Double Any) where
    type MainOpts (QDiagram Ray V3 Double Any) = (DiagramOpts, DiagramLoopOpts)
    mainRender (opts, l) d = chooseRender opts d >> defaultLoopRender l

chooseRender :: DiagramOpts -> QDiagram Ray V3 Double Any -> IO ()
chooseRender opts d =
  case splitOn "." (opts ^. output) of
    [""] -> putStrLn "No output file given."
    --atm only ppm is supported
    ps | last ps `elem` ["png","jpg","tga"] -> do
           let outTyp = case last ps of
                 "png" -> PNG
                 "jpg" -> JPG
                 "tga" -> TGA
                 _    -> error "unsuported type"
           let w = fromIntegral <$> opts^.width
           let h = fromIntegral <$> opts^.height
           renderScene (opts^.output) outTyp (fromMaybe 480 h) (fromMaybe 640 w) d
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps


multiMain :: [(String, QDiagram Ray V3 Double Any)] -> IO ()
multiMain = mainWith

instance Mainable [(String, QDiagram Ray V3 Double Any)] where
    type MainOpts [(String, QDiagram Ray V3 Double Any)]
        = (MainOpts (QDiagram Ray V3 Double Any), DiagramMultiOpts)
    mainRender = defaultMultiMainRender



animMain :: Animation Ray V3 Double -> IO ()
animMain = mainWith

instance Mainable (Animation Ray V3 Double) where
    type MainOpts (Animation Ray V3 Double) = ((DiagramOpts, DiagramAnimOpts), DiagramLoopOpts)
    mainRender (opts, l) d =  defaultAnimMainRender chooseRender output opts d >> defaultLoopRender l
