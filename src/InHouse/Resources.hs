{-# LANGUAGE OverloadedStrings #-}
module InHouse.Resources where

import Data.Map (Map)
import Control.Monad.Reader
import qualified Data.Map as M
import qualified SDL
import qualified SDL.Image as Image
import qualified SDL.Raw.Video as Raw
import qualified SDL.Internal.Numbered as Numbered
import Foreign.C.Types as FT
import InHouse.Util
import InHouse.Conf
import Data.Maybe
import Data.Foldable

assetDir = "assets"

type AssetId = String

data Sprite =
    Still
      { tId        :: AssetId
      , tTextureId :: AssetId
      , tSize      :: (Int, Int)
      , tInstances :: [((Int, Int), Maybe (Int, Int))]
      }
  | Animation
      { aId            :: AssetId
      , aTextureId     :: AssetId
      , aSize          :: (Int, Int)
      , tOff           :: Maybe (Int, Int)
      , aFrames        :: [((Int, Int), Int)]
      , aTotalDuration :: Int                 -- after how many ticks will the animation start over
      }

data AnimationInfo = AnimationInfo
  { aiId             :: AssetId
  , aiIsPlaying      :: Bool
  , aiLastUpdated    :: Int
  , aiAliveTicks     :: Int
  }

data Resources = Resources
  { rTextures    :: Map AssetId SDL.Texture
  , rSprites     :: Map AssetId Sprite
  , rMinTileSize :: (Int, Int)
  , rTileScale   :: (Int, Int)
  }

loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
  conf <- readConf $ getAssetPath "assets.yaml"
  it <- createTexturesFromConf conf renderer
  sp <- createSpritesFromConf conf
  return $ Resources it sp (48, 48) (48, 48)

createTexturesFromConf :: Conf -> SDL.Renderer -> IO (Map AssetId SDL.Texture)
createTexturesFromConf conf renderer =
  let textureDefinitions = fromJust $ getConf "textures" conf :: [[String]]
  in foldlM
       (\m [fn, n, a] ->
         fmap
           (flip
             (M.insert n)
             m)
         $ createTexture
             renderer
             fn
             $ parseColor a)
       M.empty
       textureDefinitions

createSpritesFromConf :: Conf -> IO (Map AssetId Sprite)
createSpritesFromConf conf = do
  let stills     = fromJust $ getConf "stills" conf :: [String]
      animations = fromJust $ getConf "animations" conf :: [String]
  m0 <- foldlM
          (\m n -> do
            s@(Still id _ _ _) <- createStill n
            return $ M.insert id s m)
          M.empty
          stills
  foldlM
    (\m n -> do
      a@(Animation id _ _ _ _ _) <- createAnimation n
      return $ M.insert id a m)
    m0
    animations

createStill :: String -> IO Sprite
createStill name = do
  conf <- readConf $ getAssetPath name
  let textureId = getConf "texture" conf
      name      = getConf "name" conf
      size      = getConf "size" conf
      instances = map (\x -> case x of
                               [a, b] -> ((a, b), Nothing)
                               [a, b, c, d] -> ((a, b), Just (c, d))
                               _ -> error "Wrong number of instance arguments") $ fromJust $ getConf "instances" conf
  return $ Still name textureId size instances

createAnimation :: String -> IO Sprite
createAnimation name = do
  conf <- readConf $ getAssetPath name
  let textureId = getConf "texture" conf
      name      = getConf "name" conf
      size      = getConf "size" conf
      frames    = getConf "frames" conf
      off       = tryGetConf "offset" conf
  return $ Animation name textureId size off frames (sum $ map snd frames)

--drawSprite :: (MonadIO m) => SDL.Renderer -> Resources -> AssetId -> Rectangle -> m ()
--drawSprite renderer resources name rect =
--  let (Still _ textureId (w, h) (((x, y), _):_)) = fromJust $ M.lookup name (rSprites resources)
--      texture = fromJust $ M.lookup textureId (rTextures resources)
--      from = Just $ toSDLRectangle $ Rect x y w h
--      to   = Just $ toSDLRectangle rect
--  in SDL.copy renderer texture from to

toSDLRectangle :: (Num a) => Rectangle -> SDL.Rectangle a
toSDLRectangle (Rect x y w h) = SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) (SDL.V2 (fromIntegral w) (fromIntegral h))

-- Todo: use proper Path library
getAssetPath :: String -> FilePath
getAssetPath name = assetDir ++ "/" ++ name

createTexture :: SDL.Renderer -> String -> Color -> IO SDL.Texture
createTexture renderer name (r,g,b) = do
  surface0 <- Image.load $ getAssetPath name
  surface <- convertSurface surface0 SDL.RGBA8888
  SDL.freeSurface surface0
  SDL.surfaceColorKey surface SDL.$= (Just $ SDL.V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 0x00)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return texture

convertSurface :: SDL.Surface -> SDL.PixelFormat -> IO SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt
