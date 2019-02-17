{-# language OverloadedStrings #-}
module HFish.Interpreter.Str where

import Prelude hiding (span,break,length,drop,take,takeWhile,dropWhile)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.String (IsString(..))
import Data.Semigroup
import Text.Read

-- | The /string/ type.
type Str = B.ByteString

singleton :: Char -> Str
singleton = BC.singleton

toString :: Str -> String
toString = T.unpack . decodeUtf8

fromString :: String -> Str
fromString = Data.String.fromString

toText :: Str -> T.Text
toText = decodeUtf8

fromText :: T.Text -> Str
fromText = encodeUtf8

splitWith :: (Char -> Bool) -> Str -> [Str]
splitWith = BC.splitWith

split :: Char -> Str -> [Str]
split = BC.split

splitOn :: Str -> Str -> [Str]
splitOn = splitOnBs

splitAt :: Int -> Str -> (Str,Str)
splitAt = B.splitAt

intercalate :: Str -> [Str] -> Str
intercalate = B.intercalate

words :: Str -> [Str]
words = splitOn " "

unwords :: [Str] -> Str
unwords = intercalate " "

lines :: Str -> [Str]
lines = splitOn "\n"

unlines :: [Str] -> Str
unlines = intercalate "\n"

length :: Str -> Int
length = B.length

null :: Str -> Bool
null = B.null

empty :: Str
empty = B.empty

take :: Int -> Str -> Str
take = B.take

drop :: Int -> Str -> Str
drop = B.drop

takeEnd :: Int -> Str -> Str
takeEnd i = do
  l <- length
  drop (l-i)

dropEnd :: Int -> Str -> Str
dropEnd i = do
  l <- length
  take (l-i)

find :: (Char -> Bool) -> Str -> Maybe Char
find = BC.find

span :: (Char -> Bool) -> Str -> (Str,Str)
span = BC.span

break :: (Char -> Bool) -> Str -> (Str,Str)
break = BC.break

spanEnd :: (Char -> Bool) -> Str -> (Str,Str)
spanEnd = BC.spanEnd

breakEnd :: (Char -> Bool) -> Str -> (Str,Str)
breakEnd = BC.breakEnd

takeWhile :: (Char -> Bool) -> Str -> Str
takeWhile = BC.takeWhile

dropWhile :: (Char -> Bool) -> Str -> Str
dropWhile = BC.dropWhile

dropWhileEnd :: (Char -> Bool) -> Str -> Str
dropWhileEnd p = snd . spanEnd p

takeWhileEnd :: (Char -> Bool) -> Str -> Str
takeWhileEnd p = fst . spanEnd p

dropAround :: (Char -> Bool) -> Str -> Str
dropAround p = dropWhileEnd p . dropWhile p

reverse :: Str -> Str
reverse = B.reverse

readStr :: Read a => Str -> a
readStr = read . toString

readStrMaybe :: Read a => Str -> Maybe a
readStrMaybe = readMaybe . toString

showStr :: Show a => a -> Str
showStr = HFish.Interpreter.Str.fromString . show


splitOnBs :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOnBs s = splitOff
  where
    l = B.length s
    splitOff bs
      | B.null bs = []
      | True = let (hd,tl) = B.breakSubstring s bs
                in hd : splitOff (B.drop l tl)

