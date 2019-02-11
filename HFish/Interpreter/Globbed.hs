{-# language LambdaCase, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module HFish.Interpreter.Globbed (
  Globbed(..)
  ,fromStr
  ,fromGlob
  ,fromString
  ,globExpand
  ,matchGlobbed
  ,matchStr
) where

import Fish.Lang
import HFish.Interpreter.Str (Str)
import HFish.Interpreter.Core
import HFish.Interpreter.Util
import qualified HFish.Interpreter.Str as Str

import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence
import Data.Maybe
import Data.Semigroup
import Data.String (IsString (..))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import Text.Regex.Applicative

instance Semigroup g => Semigroup (RE a g) where
  re1 <> re2 = (<>) <$> re1 <*> re2

instance (Semigroup m,Monoid m) => Monoid (RE a m) where
  mempty = pure mempty
  mappend = (<>)

newtype Globbed = Globbed {
    unGlob :: Seq (Either Glob Str)
  }
  deriving (Eq,Ord,Show,Semigroup,Monoid)


fromStr :: Str -> Globbed
fromStr t = Globbed $ pure (Right t)

fromGlob :: Glob -> Globbed
fromGlob g = Globbed $ pure (Left g)

instance IsString Globbed where
  fromString = fromStr . fromString

showGlobbed :: Globbed -> String
showGlobbed = F.fold . fmap f . unGlob
  where
    f :: Either Glob Str -> String
    f = \case
      Left g -> case g of
        StarGl -> "*"
        DiStarGl -> "**"
      Right s -> Str.toString s


globExpand :: Globbed -> Fish (Seq Str)
globExpand globbed = 
  case optimisticCast globbed of
    Just s -> pure (pure s)
    Nothing -> do
      wdir <- use cwdir
      paths <- getMatches
        <$> recurseDirRel True (Str.toString wdir)
      if Seq.null paths
        then noMatchErr globbed
        else pure . fmap fromString $ paths
  where
    parser = genParser globbed
    getMatches = Seq.filter $ isJust . (=~ parser)
    
    noMatchErr globbed = errork
      $ "No matches for glob pattern: "
        <> showGlobbed globbed

optimisticCast :: Globbed -> Maybe Str
optimisticCast = F.foldrM f "" . unGlob
  where
    f mg text = case mg of
      Left _ -> Nothing
      Right s -> Just $ s <> text

matchGlobbed :: Globbed -> Str -> Maybe String
matchGlobbed globbed str = 
  genParser globbed & (Str.toString str =~)

genParser :: Globbed -> RE Char String
genParser = F.fold . fmap f . unGlob
  where
    f = \case
      Right s -> string $ Str.toString s
      Left g -> case g of
        StarGl -> few $ psym (/='/')
        DiStarGl -> few anySym

-- Used by fishSwitch
genParserFromStr :: Str -> RE Char String
genParserFromStr = work . Str.toString
  where
    work :: String -> RE Char String
    work = \case
      [] -> pure ""
      g:gs -> (\p -> (++) <$> p <*> work gs)
        $ case g of
          '*' -> few $ anySym
          '?' -> pure <$> anySym
          _ -> pure <$> sym g

-- Used by fishSwitch
matchStr :: Str -> Str -> Maybe String
matchStr globStr str =
  genParserFromStr globStr & (Str.toString str =~)

recurseDirRel :: Bool -> FilePath -> Fish (Seq FilePath)
recurseDirRel b p = do
  wdir <- use cwdir
  paths <- liftIO (recurseDir b p)
  pure $ fmap (makeRelative $ Str.toString wdir) paths


recurseDir :: Bool -> FilePath -> IO (Seq FilePath)
recurseDir ignoreHidden p = do
  content <- Seq.fromList
    . map (p </>)
    . Prelude.filter (not . isHidden)
    <$> listDirectory p -- todo catch errors
  mpaths <- forM content continue
  pure $ content <> F.foldr catf mempty mpaths
  where
    catf mx pths = case mx of
      Just x -> x <> pths
      Nothing -> pths
    
    continue :: FilePath -> IO (Maybe (Seq FilePath))
    continue p =
      doesDirectoryExist p >>= \case
        True -> do
          files <- recurseDir ignoreHidden p
          pure $ Just files
        False -> pure Nothing
    
    isHidden p = case takeFileName p of
      '.' : _ -> True
      _ -> False
      -- head (takeFileName p) == '.'
