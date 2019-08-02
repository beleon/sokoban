module InHouse.Conf where

import           Data.HashMap.Strict           as HM
import           Data.Yaml
import           Data.Aeson
import           Data.Text                      ( pack )
import           Data.Maybe                     ( fromMaybe
                                                , fromJust
                                                )

-- Hack to avoid intermediate data structures when working with yaml configs.
type Conf = Value

readConf :: FilePath -> IO Conf
readConf = fmap (either (error . show) id) . decodeFileEither

getConf :: (FromJSON a) => String -> Conf -> a
getConf s =
  fromMaybe (error ("Key \"" ++ s ++ "\" not present in config")) . tryGetConf s

tryGetConf :: (FromJSON a) => String -> Conf -> Maybe a
tryGetConf k (Object x) = (fmap fromJSON $ HM.lookup (pack k) x) >>= \x ->
  case x of
    (Error   s) -> error s
    (Success a) -> Just a
