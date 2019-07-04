module InHouse.Conf where

import Data.HashMap.Strict as HM
import Data.Yaml
import Data.Aeson
import Data.Text (pack)

-- Hack to avoid intermediate data structures when working with yaml configs.
type Conf = Value

readConf :: FilePath -> IO Conf
readConf = fmap (either (error . show) id) . decodeFileEither

getConf :: (FromJSON a) => String -> Conf -> a
getConf k (Object x) = case fromJSON $ x HM.! (pack k) of
                         (Error s)   -> error s
                         (Success a) -> a
