{-# LANGUAGE OverloadedStrings #-}

module Shortener.Converter (
    encodeR,
    encodeL,
    decodeR,
    decodeL
) where

import Prelude
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import           GHC.Int (Int64)

-- Just letters and numbers; other characters are either reserved by the spec or by me (for other uses)
chars :: [Char]
chars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
base :: Int64
base = fromIntegral $ length chars

-- Maps for easy encoding and decoding
charsToNums :: Map.Map Char Int64
numsToChars :: Map.Map Int64 Char
(charsToNums, numsToChars) =
    let charsWithNums = zip chars [0..]
        cn = Map.fromList charsWithNums
        nc = Map.fromList $ map Tuple.swap charsWithNums
        in (cn, nc)

encodeL :: Int64 -> Maybe String
encodeL = encodeL' []
    where encodeL' _ i | i < 0 = Nothing
          encodeL' accum i = do
              digit <- Map.lookup (i `mod` base) numsToChars
              let remainder = i `div` base
                  newAccum = digit : accum
              if remainder > 0
                then
                    encodeL' newAccum remainder
                else
                    return newAccum

encodeR :: Int64 -> Maybe String
encodeR i | i < 0  = Nothing
encodeR i | i == 0 = Just []
encodeR i = do
    digit <- Map.lookup (i `mod` base) numsToChars
    let remainder = i `div` base
    fmap (digit:) $ encodeR remainder

-- Take in a string which hopefully represents a base 64 (or whatever) value, and attempt to convert it to an appropriate Int64
-- If the string contains invalid characters, Nothing is returned
decodeL :: String -> Maybe Int64
decodeL = fmap (foldl appendNum 0) . mapM (flip Map.lookup charsToNums)
     where appendNum acc n = (acc * base) + n

-- Take in a string which hopefully represents a base 64 (or whatever) value, and attempt to convert it to an appropriate Int64
-- If the string contains invalid characters, Nothing is returned
decodeR :: String -> Maybe Int64
decodeR = fmap (foldr appendNum 0) . mapM (flip Map.lookup charsToNums)
     where appendNum n acc = (acc * base) + n

