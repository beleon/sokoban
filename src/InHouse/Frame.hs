module InHouse.Frame where

import InHouse.Resources
import qualified Data.Map as M
import qualified SDL
import Control.Monad.Reader
import Data.Foldable
import Data.Either

data Frame = Frame [SpriteInstance]

data SpriteInstance =
    StillInstance
      { sInAssetId       :: AssetId
      , sInLocation      :: Rectangle
      , sInInstanceIndex :: Int
      }
  | AnimationInstance
      { aInAssetId  :: AssetId
      , aInLocation :: Rectangle
      , aInTicks    :: Int
      }

drawFrame :: (MonadIO m) => SDL.Renderer -> Resources -> Frame -> m ()
drawFrame renderer resources (Frame sis) = foldlM (\_ si -> drawSpriteInstance renderer resources si) () sis

drawSpriteInstance :: (MonadIO m) => SDL.Renderer -> Resources -> SpriteInstance -> m ()
drawSpriteInstance renderer resources si =
  let (texture, from, to) = case si of
        (StillInstance sId sLoc sInd) ->
          let (Still _ textureId (w, h) instances) = rSprites resources M.! sId
              texture = rTextures resources M.! textureId
              (x, y) = instances!!sInd
          in (texture, Rect x y w h, sLoc)
        (AnimationInstance aId aLoc aTicks) ->
          let (Animation _ textureId (w, h) frames dur) = rSprites resources M.! aId
              texture = rTextures resources M.! textureId
              (x, y) = fromRight (error "cannot happen") $ foldl (\r (p, z) -> either (\t -> if t < z then Right p else Left (t - z)) Right r) (Left $ mod aTicks dur) frames
          in (texture, Rect x y w h, aLoc)
  in SDL.copy renderer texture (Just $ toSDLRectangle from) (Just $ toSDLRectangle to)
