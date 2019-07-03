module InHouse.Conf where

import Data.HashMap.Strict as HM
import Data.Yaml
import Data.Text (pack)

-- Ugly hack to parse yaml conf to avoid intermediate data structures. Will probably replace when I find a more suitable config package that is maintained.

type Conf = Value

readConf :: FilePath -> IO Conf
readConf = fmap (either (error . show) id) . decodeFileEither

getConf :: (FromJSON a) => String -> Conf -> a
getConf k (Object x) = either (error . show) id $ decodeEither' $ encode $ HM.lookup (pack k) x
