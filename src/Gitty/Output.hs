module Gitty.Output (echo) where

import Text.Printf (PrintfType, printf)

echo :: (PrintfType r) => String -> r
echo = printf