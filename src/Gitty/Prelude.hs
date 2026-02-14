module Gitty.Prelude (WorkDir, bsToHex) where

import qualified Data.ByteString as BS
import Text.Printf (printf)

type WorkDir = FilePath

bsToHex :: BS.ByteString -> String
bsToHex = concatMap (printf "%02x") . BS.unpack