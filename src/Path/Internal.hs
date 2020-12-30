{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Path.Internal where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Sequence
import Data.String
import Data.Void
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec

type Parser = Parsec Void ByteString

data PathSeg
  = Parent
  | PathSeg {-# UNPACK #-} !ByteString
  deriving (Show, Eq, Lift, Generic)

fromPathSeg :: PathSeg -> ByteString
fromPathSeg Parent = ".."
fromPathSeg (PathSeg p) = p

pathSeg :: Parser (Maybe PathSeg)
pathSeg = try parentP <|> try dot <|> normalSeg
  where
    parentP = do
      _ <- single 46
      _ <- single 46
      pure (Just Parent)
    normalSeg = Just . PathSeg <$> takeWhile1P Nothing (/= 47)
    dot = do
      _ <- single 46
      pure Nothing

pathSeg' :: Parser (Maybe PathSeg)
pathSeg' = pathSeg <|> pure Nothing

relpath :: Parser (Path 'Rel)
relpath = do
  h <- pathSeg
  t <- many $ single 47 *> pathSeg'
  pure $ Path $ fromList $ catMaybes $ h : t

abspath :: Parser (Path 'Abs)
abspath = do
  _ <- single 47
  l <- sepBy pathSeg' $ single 47
  pure $ Path $ fromList $ catMaybes l

data PathType
  = Abs
  | Rel
  deriving (Show, Eq)

-- | A canonicalized file path
newtype Path (t :: PathType) = Path
  { unPath :: Seq PathSeg
  }
  deriving stock (Generic, Lift)
  deriving newtype (Eq, Show)

fromRel :: Path 'Rel -> ByteString
fromRel (fmap fromPathSeg . unPath -> l)
  | Empty <- l = "."
  | x :<| xs <- l = foldl' (\p s -> p <> "/" <> s) x xs

fromAbs :: Path 'Abs -> ByteString
fromAbs (fmap fromPathSeg . unPath -> l)
  | Empty <- l = "/"
  | x :<| xs <- l = "/" <> foldl' (\p s -> p <> "/" <> s) x xs

parseRel :: ByteString -> Maybe (Path 'Rel)
parseRel = parseMaybe relpath

parseAbs :: ByteString -> Maybe (Path 'Abs)
parseAbs = parseMaybe abspath

mkAbs :: ByteString -> Q Exp
mkAbs = lift . fromMaybe (error "illformed absolute path") . parseAbs

mkRel :: ByteString -> Q Exp
mkRel = lift . fromMaybe (error "illformed relative path") . parseRel

qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
    { quoteExp = quoteExp' . fromString,
      quotePat = \_ ->
        fail "illegal QuasiQuote (allowed as expression only, used as a pattern)",
      quoteType = \_ ->
        fail "illegal QuasiQuote (allowed as expression only, used as a type)",
      quoteDec = \_ ->
        fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
    }

absp :: QuasiQuoter
absp = qq mkAbs

relp :: QuasiQuoter
relp = qq mkRel

(</>) :: Path t -> Path 'Rel -> Path t
(Path p1) </> (Path p2) = Path (p1 <> p2)

stripPrefix :: Path t -> Path t -> Maybe (Path 'Rel)
stripPrefix (Path p1) (Path p2) =
  Path <$> stripPrefix' p1 p2
  where
    stripPrefix' Empty p = pure p
    stripPrefix' _ Empty = Nothing
    stripPrefix' (x :<| xs) (y :<| ys)
      | x == y = stripPrefix' xs ys
      | otherwise = Nothing

isPrefixOf :: Path t -> Path t -> Bool
isPrefixOf p1 p2 = isJust $ stripPrefix p1 p2

parent :: Path t -> Path t
parent (Path Empty) = Path Empty
parent (Path (xs :|> _)) = Path xs

filename :: Path t -> Path 'Rel
filename (Path Empty) = Path Empty
filename (Path (Empty :|> x)) = Path $ pure x
filename (Path (_ :<| xs)) = filename $ Path xs

unsafeToPath :: [ByteString] -> Path t
unsafeToPath = Path . fromList . fmap PathSeg
