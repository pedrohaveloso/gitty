module Gitty.Compression (compress, decompress) where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import Data.Function ((&))

compress :: BS.ByteString -> BS.ByteString
compress bs = bs & BS.fromStrict & Zlib.compress & BS.toStrict

decompress :: BS.ByteString -> BS.ByteString
decompress bs = bs & BS.fromStrict & Zlib.decompress & BS.toStrict
