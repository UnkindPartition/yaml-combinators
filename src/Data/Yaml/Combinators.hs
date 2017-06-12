-- | Combinators for parsing YAML into Haskell types.
--
-- Based on the article <https://ro-che.info/articles/2015-07-26-better-yaml-parsing Better Yaml Parsing>.
{-# LANGUAGE PolyKinds, DataKinds, KindSignatures,
             ExplicitForAll, TemplateHaskell, ViewPatterns,
             TypeOperators, TypeFamilies,
             GeneralizedNewtypeDeriving #-}
module Data.Yaml.Combinators
  ( Parser
  , parse
  , runParser
  -- * Scalars
  , string
  , theString
  , number
  , integer
  , bool
  -- * Arrays
  , array
  , theArray
  , ElementParser
  , element
  -- * Objects
  , object
  , FieldParser
  , field
  , optField
  , theField
  -- * Errors
  , ParseError(..)
  , Reason(..)
  ) where

import Data.Aeson (Value(..), Object, Array)
import Data.Scientific
import Data.Yaml (decodeEither, encode)
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Bifunctor (first)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State as State
import Control.Monad.Trans.Class
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Functor.Product
import Data.Functor.Constant
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Generics.SOP
import Generics.SOP.TH

-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import Data.Monoid

deriveGeneric ''Value

----------------------------------------------------------------------
--                           Parsing function
----------------------------------------------------------------------

-- | Run a 'Parser' on a 'ByteString' containing the YAML content.
--
-- This is a high-level function implemented on top of 'runParser'.
parse :: Parser a -> ByteString -> Either String a
parse p bs = do
  aesonValue <- decodeEither bs
  first ppParseError $ runParser p aesonValue

----------------------------------------------------------------------
--                      Errors and Pretty-printing
----------------------------------------------------------------------

-- | A parse error. 'Reason' describes the error.
-- The 'Int' field denotes at which level the error occurred and
-- is used to select the deepest (most relevant) error
-- when merging multiple parsers.
data ParseError = ParseError
  !Int  -- level
  Reason
  deriving (Eq, Show)

-- | Describes what exactly went wrong during parsing.
data Reason
  = UnexpectedAsPartOf Value Value
  | ExpectedAsPartOf String Value
  | ExpectedInsteadOf String Value
  deriving (Eq, Show)

mergeParseError :: ParseError -> ParseError -> ParseError
mergeParseError e1@(ParseError l1 r1) e2@(ParseError l2 r2) =
  case compare l1 l2 of
    GT -> e1
    EQ
      | ExpectedAsPartOf exp1 w1 <- r1
      , ExpectedAsPartOf exp2 w2 <- r2
      , w1 == w2
      -> ParseError l1 (ExpectedAsPartOf (exp1 ++ ", " ++ exp2) w1)
      | ExpectedInsteadOf exp1 w1 <- r1
      , ExpectedInsteadOf exp2 w2 <- r2
      , w1 == w2
      -> ParseError l1 (ExpectedInsteadOf (exp1 ++ ", " ++ exp2) w1)
    _ -> e2

ppParseError :: ParseError -> String
ppParseError (ParseError _lvl reason) =
  case reason of
    UnexpectedAsPartOf part whole ->
      "Unexpected \n\n" ++ showYaml part ++ "\nas part of\n\n" ++ showYaml whole
    ExpectedInsteadOf exp1 got ->
      "Expected " ++ exp1 ++ " instead of:\n\n" ++ showYaml got
    ExpectedAsPartOf exp1 got ->
      "Expected " ++ exp1 ++ " as part of:\n\n" ++ showYaml got
  where
    showYaml :: Value -> String
    showYaml = BS8.unpack . encode

----------------------------------------------------------------------
--                           Core definitions
----------------------------------------------------------------------

newtype ParserComponent a fs = ParserComponent (Maybe (NP I fs -> Either ParseError a))
-- | A top-level YAML parser.
--
-- * Construct a 'Parser' with 'string', 'number', 'integer', 'bool', 'array', or 'object'.
--
-- * Combine two or more 'Parser's with 'Monoid' operators
-- such as 'mappend', 'Data.Monoid.<>', or `mconcat` —
-- e.g. if you expect either an object or a string.
--
-- * Run with 'parse' or 'runParser'.
newtype Parser a = Parser (NP (ParserComponent a) (Code Value))

-- fmap for ParserComponent (in its first type argument)
pcFmap :: (a -> b) -> ParserComponent a fs -> ParserComponent b fs
pcFmap f (ParserComponent mbP) = ParserComponent $ (fmap . fmap . fmap $ f) mbP

instance Functor Parser where
  fmap f (Parser comps) = Parser $ hliftA (pcFmap f) comps

instance Monoid (ParserComponent a fs) where
  mempty = ParserComponent Nothing
  ParserComponent mbP1 `mappend` ParserComponent mbP2 =
    ParserComponent $ case (mbP1, mbP2) of
      (Nothing, Nothing) -> Nothing
      (Just p1, Nothing) -> Just p1
      (Nothing, Just p2) -> Just p2
      (Just p1, Just p2) -> Just $ \v ->
        case (p1 v, p2 v) of
          (Right r1, _) -> Right r1
          (_, Right r2) -> Right r2
          (Left l1, Left l2) -> Left $ mergeParseError l1 l2

instance Monoid (Parser a) where
  mempty = Parser $ hpure mempty
  Parser rec1 `mappend` Parser rec2 = Parser $ hliftA2 mappend rec1 rec2

-- | A low-level function to run a 'Parser'.
runParser :: Parser a -> Value -> Either ParseError a
runParser (Parser comps) orig@(from -> SOP v) =
  hcollapse $ hliftA2 match comps v
  where
    match :: ParserComponent a fs -> NP I fs -> K (Either ParseError a) fs
    match (ParserComponent mbP) v1 = K $
      case mbP of
        Nothing -> Left $ ParseError 0 $ ExpectedInsteadOf expected orig
        Just p -> p v1

    expected =
      let
        f (ParserComponent pc) (K name) = K (name <$ pc)
      in intercalate ", " . catMaybes . hcollapse $ hliftA2 f comps valueConNames

valueConNames :: NP (K String) (Code Value)
valueConNames =
  let
    ADT _ _ cons = datatypeInfo (Proxy :: Proxy Value)
  in hliftA (\(Constructor name) -> K name) cons


fromComponent :: forall a . NS (ParserComponent a) (Code Value) -> Parser a
fromComponent parser = Parser $ hexpand mempty parser

----------------------------------------------------------------------
--                           Combinators
----------------------------------------------------------------------

incErrLevel :: Either ParseError a -> Either ParseError a
incErrLevel = first $ \(ParseError l r) -> ParseError (l+1) r

-- | Match a single YAML string.
--
-- >>> parse string "howdy"
-- Right "howdy"
string :: Parser Text
string = fromComponent $ S . S . Z $ ParserComponent $ Just $ \(I s :* Nil) -> Right s

-- | Match a specific YAML string, usually a «tag» identifying a particular
-- form of an array or object.
--
-- >>> parse (theString "hello") "hello"
-- Right ()
-- >>> either putStr print $ parse (theString "hello") "bye"
-- Expected "hello" instead of:
-- <BLANKLINE>
-- bye
theString :: Text -> Parser ()
theString t = fromComponent $ S . S . Z $ ParserComponent $ Just $ \(I s :* Nil) ->
  if s == t
    then Right ()
    else Left $ ParseError 1 (ExpectedInsteadOf (show t) (String s))

-- | Match an array of elements, where each of elements are matched by
-- the same parser. This is the function you'll use most of the time when
-- parsing arrays, as they are usually homogeneous.
--
-- >>> parse (array string) "[a,b,c]"
-- Right ["a","b","c"]
array :: Parser a -> Parser (Vector a)
array p = fromComponent $ S . Z $ ParserComponent $ Just $ \(I a :* Nil) -> incErrLevel $ mapM (runParser p) a

-- | An 'ElementParser' describes how to parse a fixed-size array
-- where each positional element has its own parser.
--
-- This can be used to parse heterogeneous tuples represented as YAML
-- arrays.
--
-- * Construct an 'ElementParser' with 'element' and the 'Applicative' combinators.
--
-- * Turn a 'FieldParser' into a 'Parser' with 'theArray'.
newtype ElementParser a = ElementParser (StateT [Value] (Either (Array -> ParseError)) a)
  deriving (Functor, Applicative)

-- | Construct an 'ElementParser' that parses the current array element
-- with the given 'Parser'.
element :: Parser a -> ElementParser a
element p = ElementParser $ do
  vs <- State.get
  case vs of
    [] -> lift $ Left $ \arr ->
      let n = V.length arr + 1
      in ParseError 0 $ ExpectedAsPartOf ("at least " ++ show n ++ " elements") $ Array arr
    (v:vs') -> do
      State.put vs'
      lift $ first const $ incErrLevel $ runParser p v

-- | Match an array consisting of a fixed number of elements. The way each
-- element is parsed depends on its position within the array and
-- is determined by the 'ElementParser'.
--
-- >>> parse (theArray $ (,) <$> element string <*> element bool) "[f, true]"
-- Right ("f",True)
theArray :: ElementParser a -> Parser a
theArray (ElementParser ep) = fromComponent $ S . Z $ ParserComponent $ Just $ \(I a :* Nil) -> incErrLevel $
  case runStateT ep (V.toList a) of
    Right (r, []) -> return r
    Right (_, v:_) -> Left $ ParseError 0 $ UnexpectedAsPartOf v $ Array a
    Left errFn -> Left $ errFn a

-- | Match a real number.
--
-- >>> parse number "3.14159"
-- Right 3.14159
number :: Parser Scientific
number = fromComponent $ S . S . S . Z $ ParserComponent $ Just $ \(I n :* Nil) -> Right n

-- | Match an integer.
--
-- >>> parse (integer @Int) "2017"
-- Right 2017
integer :: (Integral i, Bounded i) => Parser i
integer = fromComponent $ S . S . S . Z $ ParserComponent $ Just $ \(I n :* Nil) ->
  case toBoundedInteger n of
    Just i -> Right i
    Nothing -> Left $ ParseError 0 $ ExpectedInsteadOf "integer" (Number n)

-- | Match a boolean.
--
-- >>> parse bool "yes"
-- Right True
bool :: Parser Bool
bool = fromComponent $ S . S . S . S . Z $ ParserComponent $ Just $ \(I b :* Nil) -> Right b

-- | A 'FieldParser' describes how to parse an object.
--
-- * Construct a 'FieldParser' with 'field', 'optField', or 'theField', and the 'Applicative' combinators.
--
-- * Turn a 'FieldParser' into a 'Parser' with 'object'.
newtype FieldParser a = FieldParser
  (Product
    (ReaderT Object (Either ParseError))
    (Constant (HashMap Text ())) a)
  deriving (Functor, Applicative)

-- | Require an object field with the given name and with a value matched by
-- the given 'Parser'.
field
  :: Text -- ^ field name
  -> Parser a -- ^ value parser
  -> FieldParser a
field name p = FieldParser $
  Pair
    (ReaderT $ \o ->
      case HM.lookup name o of
        Nothing -> Left $ ParseError 0 $ ExpectedAsPartOf ("field " ++ show name) $ Object o
        Just v -> incErrLevel $ runParser p v
    )
    (Constant $ HM.singleton name ())

-- | Declare an optional object field with the given name and with a value
-- matched by the given 'Parser'.
optField
  :: Text -- ^ field name
  -> Parser a -- ^ value parser
  -> FieldParser (Maybe a)
optField name p = FieldParser $
  Pair
    (ReaderT $ \o -> traverse (incErrLevel . runParser p) $ HM.lookup name o)
    (Constant $ HM.singleton name ())

-- | Require an object field with the given name and the given string value.
--
-- This is a convenient wrapper around 'theString' intended for «tagging»
-- objects.
--
-- >>> :{
--     let p = object (Right <$ theField "type" "number" <*> field "value" number)
--          <> object (Left  <$ theField "type" "string" <*> field "value" string)
-- >>> :}
--
-- >>> parse p "{type: string, value: abc}"
-- Right (Left "abc")
-- >>> parse p "{type: number, value: 123}"
-- Right (Right 123.0)
theField
  :: Text -- ^ key name
  -> Text -- ^ expected value
  -> FieldParser ()
theField key value = field key (theString value)

-- | Match an object. Which set of keys to expect and how their values
-- should be parsed is determined by the 'FieldParser'.
--
-- >>> let p = object $ (,) <$> field "name" string <*> optField "age" (integer @Int)
-- >>> parse p "{ name: Anton, age: 2 }"
-- Right ("Anton",Just 2)
-- >>> parse p "name: Roma"
-- Right ("Roma",Nothing)
object :: FieldParser a -> Parser a
object (FieldParser (Pair (ReaderT parseFn) (Constant names))) = fromComponent $ Z $ ParserComponent $ Just $ \(I o :* Nil) ->
  incErrLevel $
    parseFn o <*
    (case HM.keys (HM.difference o names) of
      [] -> pure ()
      name : _ ->
        let v = o HM.! name
        in Left $ ParseError 0 $ UnexpectedAsPartOf (Object (HM.singleton name v)) (Object o)
    )
