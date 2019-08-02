module InHouse.Frame where

import           InHouse.Resources
import           InHouse.Util
import qualified Data.Map                      as M
import qualified SDL
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Either
import           Data.Maybe                     ( fromMaybe )
import           Control.Arrow                  ( second )

data Frame = Frame [[SpriteInstance]]

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
drawFrame renderer resources (Frame layers) =
  mapM_ (mapM_ (drawSpriteInstance renderer resources)) layers

drawSpriteInstance
  :: (MonadIO m) => SDL.Renderer -> Resources -> SpriteInstance -> m ()
drawSpriteInstance renderer resources si =
  let
    (texture, from, to) = case si of
      (StillInstance sId sLoc sInd) ->
        let
          (Still _ textureId (w, h) instances) = rSprites resources M.! sId
          texture       = rTextures resources M.! textureId
          (V2 toW toH)  = dim sLoc
          scale         = V2 (mod toW w) (mod toH h)
          ((x, y), off) = second ((* scale) . fromTuple . fromMaybe (0, 0))
                                 (instances !! sInd)
        in
          (texture, Rect x y w h, moveRect sLoc off)
      (AnimationInstance aId aLoc aTicks) ->
        let
          (Animation _ textureId (w, h) off frames dur) =
            rSprites resources M.! aId
          texture      = rTextures resources M.! textureId
          (V2 toW toH) = dim aLoc
          scale        = V2 (mod toW w) (mod toH h)
          (x, y)       = fromRight (error "cannot happen") $ foldl
            (\r (p, z) ->
              either (\t -> if t < z then Right p else Left (t - z)) Right r
            )
            (Left $ mod aTicks dur)
            frames
          off' = (* scale) $ fromTuple $ fromMaybe (0, 0) off
        in
          (texture, Rect x y w h, moveRect aLoc off')
  in  SDL.copy renderer
               texture
               (Just $ toSDLRectangle from)
               (Just $ toSDLRectangle to)
