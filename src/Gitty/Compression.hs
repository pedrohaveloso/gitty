module Gitty.Compression (compress, decompress) where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as ByteString
import Data.Function ((&))

compress :: ByteString.ByteString -> ByteString.ByteString
compress bs = bs & ByteString.fromStrict & Zlib.compress & ByteString.toStrict

decompress :: ByteString.ByteString -> ByteString.ByteString
decompress bs = bs & ByteString.fromStrict & Zlib.decompress & ByteString.toStrict
