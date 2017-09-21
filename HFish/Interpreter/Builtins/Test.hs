{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Test (
  testF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Util
import HFish.Interpreter.Status
import qualified HFish.Interpreter.Stringy as Str

import Data.Semigroup
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception

import qualified Data.Text as T
import qualified Data.Char as C

import System.Posix.Types
import System.Posix.Files
import System.Posix.User
import System.Posix.Terminal (queryTerminal)

import Text.Parser.Expression
import Data.Attoparsec.ByteString.Char8
import Text.Parser.Char (alphaNum)


testF :: Builtin
testF _ ts = do
  f <- parseTestE (Str.unwords ts)
  b <- liftIO $ catch f onIOErr
  if b then ok else bad
  where
    -- IO errors get mapped to False. This is usually
    -- what you want and agrees with the fish implementation.
    onIOErr :: IOException -> IO Bool
    onIOErr = const (pure False)
    

{- File Operations -}
hasMode ::
  FileMode -- the mode to check for
  -> FileMode -- the mode to be checked
  -> Bool
hasMode m1 m2 =
  (m2 `intersectFileModes` m1) == m1

hasGidSet :: FileStatus -> Bool
hasGidSet =
  hasMode setGroupIDMode
  . fileMode

hasUidSet :: FileStatus -> Bool
hasUidSet =
  hasMode setUserIDMode
  . fileMode

isUserOwned :: FileStatus -> IO Bool
isUserOwned stat = 
  (==fileOwner stat)
  <$> getEffectiveUserID
  
isUserGroup :: FileStatus -> IO Bool
isUserGroup stat =
  (==fileGroup stat)
  <$> getEffectiveGroupID

isTtyFd :: Int -> IO Bool
isTtyFd =
  queryTerminal
  . toEnum

isReadable :: FilePath -> IO Bool
isReadable p =
  fileAccess p True False False

isWriteable :: FilePath -> IO Bool
isWriteable p =
  fileAccess p False True False

isExecutable :: FilePath -> IO Bool
isExecutable p =
  fileAccess p False False True

{- Main Parsers -}

fileE :: Parser (IO Bool)
fileE = file1E <|> file2E <|> file3E <|> file4E
  where
    file = Str.toString
      <$> takeWhile1 (not . C.isSpace)
      <* skipSpace

    file4E = do
      f <- sym "-L" $> isSymbolicLink
      p <- file
      pure (f <$> getSymbolicLinkStatus p)

    file1E = do
      f <- choice
        [ sym "-b" $> isBlockDevice
         ,sym "-c" $> isCharacterDevice
         ,sym "-d" $> isDirectory
         ,sym "-f" $> isRegularFile
         ,sym "-S" $> isSocket
         ,sym "-p" $> isNamedPipe
         ,sym "-s" $> fmap (>0) fileSize
         ]
      p <- file
      pure (f <$> getFileStatus p)
     
    file2E = do
      f <- choice
        [ sym "-e" $> fileExist
         ,sym "-r" $> isReadable
         ,sym "-w" $> isWriteable
         ,sym "-x" $> isExecutable ]
      p <- file
      pure (f p)

    file3E = do
      f <- choice
        [ sym "-g" $> (pure . hasGidSet)
         ,sym "-u" $> (pure . hasUidSet)
         ,sym "-G" $> isUserGroup
         ,sym "-O" $> isUserOwned
         ]
      p <- file
      pure (f =<< getFileStatus p)

strE :: Parser Bool
strE = strUnary <|> strBinary
  where
    str = Str.toString
      <$> takeWhile1 (not . C.isSpace)
      <* skipSpace
    strBinary = do
      s <- str
      f <- choice
        [ sym "=" $> (==)
          ,sym "!=" $> (/=) ]
      f s <$> str
    
    strUnary = do
      f <- choice
        [ sym "-n" $> (/="")
         ,sym "-z" $> (=="") ]
      f <$> takeTill C.isSpace <* skipSpace

ttyTestE :: Parser (IO Bool)
ttyTestE = "-t" *> int
  >>= (pure . isTtyFd)

numE :: Parser Bool
numE = do
  i <- int
  f <- choice
    [ sym "-eq" $> (==)
     ,sym "-ne" $> (/=)
     ,sym "-gt" $> (>)
     ,sym "-ge" $> (>=)
     ,sym "-lt" $> (<)
     ,sym "-le" $> (<=) ]
  f i <$> int

{- Parsing -}

prefix s f = Prefix (f <$ sym s <* skipSpace)
postfix s f = Postfix (f <$ sym s <* skipSpace)
binary s f assoc = Infix (f <$ sym s <* skipSpace) assoc

opTable =
  [ [ prefix "!" (fmap not) ]
   ,[ binary "-a" (liftM2 (&&)) AssocRight ]
   ,[ binary "-o" (liftM2 (||)) AssocLeft ] ]

term :: Parser (IO Bool)
term =
  ( bracketed
    <|> (pure <$> numE)
    <|> fileE
    <|> (pure <$> strE) )
  <* skipSpace
  where
  bracketed = char '(' *> testE <* char ')'

testE :: Parser (IO Bool)
testE = skipSpace *> buildExpressionParser opTable term

parseTestE :: Str -> Fish (IO Bool)
parseTestE t = either onErr pure
  $ parseOnly (testE <* endOfInput) t
  where
    onErr r = errork $ "test: malformed expression"

int :: Parser Int
int = signed decimal <* skipSpace

sym :: Str -> Parser ()
sym s = string s *> skipSpace

