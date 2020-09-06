-- | Combinators for parsing YAML into Haskell types.
--
-- Based on the article <https://ro-che.info/articles/2015-07-26-better-yaml-parsing Better Yaml Parsing>.
{-# LANGUAGE PolyKinds, DataKinds, KindSignatures,
             ExplicitForAll, TemplateHaskell, ViewPatterns,
             ScopedTypeVariables, TypeOperators, TypeFamilies,
             GeneralizedNewtypeDeriving, GADTs, LambdaCase #-}
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
  , null_
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
  , defaultField
  , theField
  , extraFields
  -- * Arbitrary values
  , anyValue
  -- * Errors
  , ParseError(..)
  , ppParseError
  , Reason(..)
  , validate
  ) where

import Data.Aeson (Value(..), Object, Array)
import Data.Scientific
import Data.Yaml (decodeEither', encode)
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Bifunctor (first)
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State as State
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Ord
import Data.Monoid
import Generics.SOP
import Generics.SOP.TH
import Data.Yaml.Combinators.Free as Free

-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import Data.Semigroup

-- orphan Value instances
deriveGeneric ''Value

----------------------------------------------------------------------
--                           Parsing function
----------------------------------------------------------------------

-- | Run a 'Parser' on a 'ByteString' containing the YAML content.
--
-- This is a high-level function implemented on top of 'runParser'.
parse :: Parser a -> ByteString -> Either String a
parse p bs = do
  aesonValue <- first show $ decodeEither' bs
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
  -- NB: the order of constructors is important for the Ord instance
  = UnexpectedAsPartOf Value Value
  | ExpectedAsPartOf (HashSet String) Value
  | ExpectedInsteadOf (HashSet String) Value
  deriving (Eq, Show)

-- | Find out which error is more severe
compareSeverity :: ParseError -> ParseError -> Ordering
compareSeverity (ParseError l1 r1) (ParseError l2 r2) =
  -- extra stuff is always less severe than mismatching/missing stuff
  comparing (not . isUnexpected) r1 r2 <>
  -- otherwise, compare the depths
  compare l1 l2 <>
  -- if the depths are equal, mismatches are more severe that misses,
  comparing isMismatch r1 r2
  where
    isUnexpected e = case e of
      UnexpectedAsPartOf {} -> True
      _ -> False
    isMismatch e = case e of
      ExpectedInsteadOf {} -> True
      _ -> False

-- | Choose the more severe of two errors.
--
-- If they are equally severe, pick the earlier one.
moreSevere :: ParseError -> ParseError -> ParseError
moreSevere e1 e2 =
  case compareSeverity e1 e2 of
    LT -> e2
    _ -> e1

-- | Choose the less severe of two errors.
--
-- If they are equally severe, pick the earlier one.
lessSevere :: ParseError -> ParseError -> ParseError
lessSevere e1 e2 =
  case compareSeverity e1 e2 of
    GT -> e2
    _ -> e1

newtype Validation a = Validation { getValidation :: Either ParseError a }
  deriving Functor

instance Applicative Validation where
  pure = Validation . Right
  Validation a <*> Validation b = Validation $
    case a of
      Right va -> fmap va b
      Left ea -> either (Left . moreSevere ea) (const $ Left ea) b

bindV :: Validation a -> (a -> Validation b) -> Validation b
bindV a b = Validation $ getValidation a >>= getValidation . b

mergeParseError :: ParseError -> ParseError -> ParseError
mergeParseError e1@(ParseError l1 r1) e2@(ParseError l2 r2)
  -- first, see if we can merge the two errors
  | l1 == l2
  , ExpectedAsPartOf exp1 w1 <- r1
  , ExpectedAsPartOf exp2 w2 <- r2
  , w1 == w2
  = ParseError l1 (ExpectedAsPartOf (exp1 <> exp2) w1)
  | l1 == l2
  , ExpectedInsteadOf exp1 w1 <- r1
  , ExpectedInsteadOf exp2 w2 <- r2
  , w1 == w2
  = ParseError l1 (ExpectedInsteadOf (exp1 <> exp2) w1)
  -- otherwise, just choose the least severe one,
  -- since its branch is more likely to be the right one
  | otherwise = lessSevere e1 e2

-- | Pretty-print a 'ParseError'
--
-- @since 1.1
ppParseError :: ParseError -> String
ppParseError (ParseError _lvl reason) =
  case reason of
    UnexpectedAsPartOf part whole ->
      "Unexpected \n\n" ++ showYaml part ++ "\nas part of\n\n" ++ showYaml whole
    ExpectedInsteadOf exp1 got ->
      "Expected " ++ fmt_list exp1 ++ " instead of:\n\n" ++ showYaml got
    ExpectedAsPartOf exp1 got ->
      "Expected " ++ fmt_list exp1 ++ " as part of:\n\n" ++ showYaml got
  where
    showYaml :: Value -> String
    showYaml = BS8.unpack . encode

    fmt_list :: HashSet String -> String
    fmt_list = intercalate ", " . sort . HS.toList

----------------------------------------------------------------------
--                           Core definitions
----------------------------------------------------------------------

newtype ParserComponent a fs = ParserComponent (Maybe (Value -> NP I fs -> Validation a))
-- | A top-level YAML parser.
--
-- * Construct a 'Parser' with 'string', 'number', 'integer', 'bool', 'array', or 'object'.
--
-- * Combine two or more 'Parser's with 'Monoid' or 'Semigroup' operators
-- such as 'mappend', '<>', or `mconcat` —
-- e.g. if you expect either an object or a string.
--
-- * Run with 'parse' or 'runParser'.
newtype Parser a = Parser (NP (ParserComponent a) (Code Value))

-- fmap for ParserComponent (in its first type argument)
pcFmap :: (a -> b) -> ParserComponent a fs -> ParserComponent b fs
pcFmap f (ParserComponent mbP) = ParserComponent $ (fmap . fmap . fmap . fmap $ f) mbP

instance Functor Parser where
  fmap f (Parser comps) = Parser $ hliftA (pcFmap f) comps

instance Semigroup (ParserComponent a fs) where
  ParserComponent mbP1 <> ParserComponent mbP2 =
    ParserComponent $ case (mbP1, mbP2) of
      (Nothing, Nothing) -> Nothing
      (Just p1, Nothing) -> Just p1
      (Nothing, Just p2) -> Just p2
      (Just p1, Just p2) -> Just $ \o v -> Validation $
        case (getValidation $ p1 o v, getValidation $ p2 o v) of
          (Right r1, _) -> Right r1
          (_, Right r2) -> Right r2
          (Left l1, Left l2) -> Left $ mergeParseError l1 l2

instance Monoid (ParserComponent a fs) where
  mempty = ParserComponent Nothing
  mappend = (<>)

instance Semigroup (Parser a) where
  Parser rec1 <> Parser rec2 = Parser $ hliftA2 mappend rec1 rec2

instance Monoid (Parser a) where
  mempty = Parser $ hpure mempty
  mappend = (<>)

-- | A low-level function to run a 'Parser'.
runParser :: Parser a -> Value -> Either ParseError a
runParser p = getValidation . runParserV p

runParserV :: Parser a -> Value -> Validation a
runParserV (Parser comps) orig@(from -> SOP v) =
  hcollapse $ hliftA2 match comps v
  where
    match :: ParserComponent a fs -> NP I fs -> K (Validation a) fs
    match (ParserComponent mbP) v1 = K $
      case mbP of
        Nothing -> Validation . Left $ ParseError 0 $ ExpectedInsteadOf (HS.singleton expected) orig
        Just p -> p orig v1

    expected =
      let
        f (ParserComponent pc) (K name) = K (name <$ pc)
      in intercalate ", " . catMaybes . hcollapse $ hliftA2 f comps valueConNames

valueConNames :: NP (K String) (Code Value)
valueConNames =
  let
    ADT _ _ cons _ = datatypeInfo (Proxy :: Proxy Value)
  in hliftA (\(Constructor name) -> K name) cons


fromComponent :: forall a . NS (ParserComponent a) (Code Value) -> Parser a
fromComponent parser = Parser $ hexpand mempty parser

-- Wrap a parser with a decorator. The decorator has access to the parsed value as well
-- as the original and can inject its own processing logic.
decorate :: forall a b. Parser a -> (a -> Value -> Either ParseError b) -> Parser b
decorate (Parser components) decorator = Parser $ hmap wrap components
  where
    wrap :: ParserComponent a fs -> ParserComponent b fs
    wrap (ParserComponent maybeP) = ParserComponent $
      case maybeP of
        Nothing -> Nothing
        Just p -> Just $ \orig val -> p orig val `bindV`
          \parsed -> Validation $ decorator parsed orig

----------------------------------------------------------------------
--                           Combinators
----------------------------------------------------------------------

incErrLevel :: Validation a -> Validation a
incErrLevel = Validation . first (\(ParseError l r) -> ParseError (l+1) r) . getValidation

-- | Match a single YAML string.
--
-- >>> parse string "howdy"
-- Right "howdy"
string :: Parser Text
string = fromComponent $ S . S . Z $ ParserComponent $ Just $ const $ \(I s :* Nil) -> pure s

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
theString t = fromComponent $ S . S . Z $ ParserComponent $ Just $ const $ \(I s :* Nil) ->
  Validation $ if s == t
    then Right ()
    else Left $ ParseError 0 (ExpectedInsteadOf (HS.singleton $ show t) (String s))

-- | Match an array of elements, where each of elements are matched by
-- the same parser. This is the function you'll use most of the time when
-- parsing arrays, as they are usually homogeneous.
--
-- >>> parse (array string) "[a,b,c]"
-- Right ["a","b","c"]
array :: Parser a -> Parser (Vector a)
array p = fromComponent $ S . Z $ ParserComponent $ Just $ const $ \(I a :* Nil) -> incErrLevel $ traverse (runParserV p) a

-- | An 'ElementParser' describes how to parse a fixed-size array
-- where each positional element has its own parser.
--
-- This can be used to parse heterogeneous tuples represented as YAML
-- arrays.
--
-- * Construct an 'ElementParser' with 'element' and the 'Applicative' combinators.
--
-- * Turn a 'FieldParser' into a 'Parser' with 'theArray'.
newtype ElementParser a = ElementParser
  (((State [Value]) :.: (ReaderT Array Validation)) a)
  deriving (Functor, Applicative)

-- | Construct an 'ElementParser' that parses the current array element
-- with the given 'Parser'.
element :: Parser a -> ElementParser a
element p = ElementParser $ Comp $ do
  vs <- State.get
  case vs of
    [] -> return $ ReaderT $ \arr -> Validation . Left $
      let n = V.length arr + 1
      in ParseError 0 $ ExpectedAsPartOf (HS.singleton $ "at least " ++ show n ++ " elements") $ Array arr
    (v:vs') -> do
      State.put vs'
      return . liftR $ runParserV p v

-- | Match an array consisting of a fixed number of elements. The way each
-- element is parsed depends on its position within the array and
-- is determined by the 'ElementParser'.
--
-- >>> parse (theArray $ (,) <$> element string <*> element bool) "[f, true]"
-- Right ("f",True)
theArray :: ElementParser a -> Parser a
theArray (ElementParser (Comp ep)) = fromComponent $ S . Z $ ParserComponent $ Just $ const $ \(I a :* Nil) -> incErrLevel $
  case first (flip runReaderT a) $ runState ep (V.toList a) of
    (result, leftover) ->
      result <*
      (case leftover of
        [] -> pure ()
        v : _ -> Validation . Left $ ParseError 0 $ UnexpectedAsPartOf v $ Array a
      )

-- | Match a real number.
--
-- >>> parse number "3.14159"
-- Right 3.14159
number :: Parser Scientific
number = fromComponent $ S . S . S . Z $ ParserComponent $ Just $ const $ \(I n :* Nil) -> pure n

-- | Match an integer.
--
-- >>> parse (integer @Int) "2017"
-- Right 2017
integer :: (Integral i, Bounded i) => Parser i
integer = fromComponent $ S . S . S . Z $ ParserComponent $ Just $ const $ \(I n :* Nil) ->
  case toBoundedInteger n of
    Just i -> pure i
    Nothing -> Validation . Left $ ParseError 0 $ ExpectedInsteadOf (HS.singleton "integer") (Number n)

-- | Match a boolean.
--
-- >>> parse bool "yes"
-- Right True
bool :: Parser Bool
bool = fromComponent $ S . S . S . S . Z $ ParserComponent $ Just $ const $ \(I b :* Nil) -> pure b

-- | Match the @null@ value.
--
-- >>> parse null_ "null"
-- Right ()
--
-- @since 1.1
null_ :: Parser ()
null_ = fromComponent $ S . S . S . S . S . Z $ ParserComponent $ Just $ const $ \Nil -> pure ()

-- | Make a parser match only valid values.
--
-- If the validator does not accept the value, it should return a
-- 'Left' 'String' with a noun phrase that characterizes the expected
-- value, as in the example:
--
-- >>> let acceptEven n = if even n then Right n else Left "an even number"
-- >>> either putStr print $ parse (integer @Int `validate` acceptEven) "2017"
-- Expected an even number instead of:
-- <BLANKLINE>
-- 2017
--
-- @since 1.0.1
validate ::
  Parser a -- ^ parser to wrap
  -> (a -> Either String b) -- ^ validator
  -> Parser b
validate parser validator =
  decorate parser (validity . validator)
  where
    validity (Right result) _    = Right result
    validity (Left problem) orig = Left $ ParseError 1 $ ExpectedInsteadOf (HS.singleton problem) orig

-- | A 'FieldParser' describes how to parse an object.
--
-- * Construct a 'FieldParser' with 'field', 'optField', or 'theField', and the 'Applicative' combinators.
--
-- * Turn a 'FieldParser' into a 'Parser' with 'object'.
newtype FieldParser a = FieldParser
  (Free FieldParserBase a)
  deriving (Functor, Applicative)

data FieldParserBase a where
  OneField
    :: Text -- field name
    -> ReaderT Object Validation a
    -> FieldParserBase a
  ExtraFields :: FieldParserBase Object

-- | Require an object field with the given name and with a value matched by
-- the given 'Parser'.
field
  :: Text -- ^ field name
  -> Parser a -- ^ value parser
  -> FieldParser a
field name p = FieldParser . Free.lift . OneField name $
  ReaderT $ \o ->
    case HM.lookup name o of
      Nothing -> Validation . Left $ ParseError 0 $ ExpectedAsPartOf (HS.singleton $ "field " ++ show name) $ Object o
      Just v -> runParserV p v


-- | Declare an optional object field with the given name and with a value
-- matched by the given 'Parser'.
optField
  :: Text -- ^ field name
  -> Parser a -- ^ value parser
  -> FieldParser (Maybe a)
optField name p = FieldParser . Free.lift . OneField name $
  ReaderT $ \o -> traverse (runParserV p) $ HM.lookup name o

-- | Declare an optional object field with the given name and with a default
-- to use if the field is absent.
defaultField
  :: Text -- ^ field name
  -> a -- ^ default value
  -> Parser a -- ^ value parser
  -> FieldParser a
defaultField name defaultVal p = fromMaybe defaultVal <$> optField name p

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

-- | This combinator does two things:
--
-- 1. Allow extra fields (not specified by 'field', 'theField' etc.) in the
-- parsed object.
-- 2. Return such extra fields as an 'Object'.
--
-- The return value can be of course ignored.
--
-- >>> let fp = field "name" string
-- >>> either putStr print $ parse (object fp) "name: Anton"
-- "Anton"
-- >>> either putStr print $ parse (object fp) "{name: Anton, age: 2}"
-- Unexpected
-- <BLANKLINE>
-- age: 2
-- <BLANKLINE>
-- as part of
-- <BLANKLINE>
-- age: 2
-- name: Anton
-- >>> either putStr print $ parse (object $ (,) <$> fp <*> extraFields) "{name: Anton, age: 2}"
-- ("Anton",fromList [("age",Number 2.0)])
-- >>> either putStr print $ parse (object $ fp <* extraFields) "{name: Anton, age: 2}"
-- "Anton"
--
-- @since 1.1.2
extraFields :: FieldParser Object
extraFields = FieldParser . Free.lift $ ExtraFields

data StrictPair a b = StrictPair !a !b

instance (Semigroup a, Semigroup b) => Semigroup (StrictPair a b) where
  StrictPair a1 b1 <> StrictPair a2 b2 = StrictPair (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (StrictPair a b) where
  mempty = StrictPair mempty mempty

-- | Match an object. Which set of keys to expect and how their values
-- should be parsed is determined by the 'FieldParser'.
--
-- >>> let p = object $ (,) <$> field "name" string <*> optField "age" (integer @Int)
-- >>> parse p "{ name: Anton, age: 2 }"
-- Right ("Anton",Just 2)
-- >>> parse p "name: Roma"
-- Right ("Roma",Nothing)
--
-- By default, this function will fail when there are unrecognized fields
-- in the object. See 'extraFields' for a way to capture or ignore them.
object :: FieldParser a -> Parser a
object (FieldParser fp) = fromComponent $ Z $ ParserComponent $ Just $ const $ \(I o :* Nil) ->
  incErrLevel $
    let
      -- Do a first run over the free FieldParser applicative to collect
      -- some metainformation: which fields are requested by the parser,
      -- and whether extra fields are requested too (and therefore allowed)
      StrictPair requested_names (Any requested_extra_fields) = Free.foldMap (\case
        OneField name _ -> StrictPair (HM.singleton name ()) (Any False)
        ExtraFields -> StrictPair mempty (Any True)
        ) fp
      extra_fields = HM.difference o requested_names
      extra_fields_error =
        when (not requested_extra_fields && not (HM.null extra_fields)) $
          Validation . Left $ ParseError 0 $
            UnexpectedAsPartOf (Object extra_fields) (Object o)
    in
      Free.run (\case
        OneField _ p -> runReaderT p o
        ExtraFields -> pure extra_fields
        ) fp
        -- See Note [Extra fields error]
        <* extra_fields_error

{- Note [Extra fields error]
   ~~~~~~~~~~~~~~~~~~~~~~~~~
   We could have written

     if not requested_extra_fields && not (HM.null extra_fields)
        then Validation . Left $  ...
        else ...

   However, we intentionally try to run the applicative parser even when
   there are extra fields, because some of the resulting validation errors
   may be more severe/interesting than the "extra fields" error.
-}

-- | Match any JSON value and return it as Aeson's 'Value'.
--
-- >>> parse anyValue "[one, two, {three: four}]"
-- Right (Array [String "one",String "two",Object (fromList [("three",String "four")])])
--
-- @since 1.1.1
anyValue :: Parser Value
anyValue = Parser $ hpure $ ParserComponent . Just $ \val _np -> pure val

-- | Like 'lift' for 'ReaderT', but doesn't require a 'Monad' instance
liftR :: f a -> ReaderT r f a
liftR = ReaderT . const
