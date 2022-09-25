module Language.JsonML

import public Language.JSON
import Language.JSON.Interfaces

import public Data.SortedMap

%default total

export
data AttributeValue
  = AVString String
  | AVNumber Double
  | AVBool Bool
  | AVNull

mutual
  -- Do not public export for validation on construction
  export
  record Tag where
    constructor MkTag
    name        : String
    attributes  : SortedMap String AttributeValue
    elementList : List Element -- Recursive use of Element

  %name Tag tag

  public export
  data Element = ETag Tag | EString String

  %name Element elem

export
implementation Show AttributeValue where
  showPrec p (AVString str) = showCon p "AVString" $ showArg str 
  showPrec p (AVNumber dbl) = showCon p "AVNumber" $ showArg dbl 
  showPrec p (AVBool x)     = showCon p "AVBool"   $ showArg x     
  showPrec _ AVNull         = "AVNull"

mutual
  showPrecTag : Prec -> Tag -> String
  showPrecTag p (MkTag name attributes elementList)
    = showCon p "MkTag"
    $ showArg name ++ showArg attributes ++ " " ++ showListElement elementList

  -- referring to Prelude.Show
  showListElement : List Element -> String
  showListElement elems = "[" ++ show' "" elems ++ "]"
    where show' : String -> List Element -> String
          show' acc []        = acc
          show' acc [x]       = acc ++ showPrecElement Open x
          show' acc (x :: xs) = show' (acc ++ showPrecElement Open x ++ ", ") xs

  showPrecElement : Prec -> Element -> String
  showPrecElement p (ETag tag)    = showCon p "ETag"    $ " " ++ showPrecTag App tag
  showPrecElement p (EString str) = showCon p "EString" $ showArg str

export
implementation Show Tag where
  showPrec = showPrecTag

export
implementation Show Element where
  showPrec = showPrecElement

export
implementation Eq AttributeValue where
  (AVString str1) == (AVString str2)  = str1 == str2
  (AVNumber dbl1) == (AVNumber dbl2)  = dbl1 == dbl2
  (AVBool x)      == (AVBool y)       = x    == y
  AVNull          == AVNull           = True
  _               == _                = False

mutual
  export
  implementation Eq Tag where
    (MkTag n1 a1 e1) == (MkTag n2 a2 e2) = n1 == n2 && a1 == a2 && e1 == e2

  export
  implementation Eq Element where
    -- TODO: Remove assert_equal
    (ETag tag1)    == (ETag tag2)     = assert_total $ tag1 == tag2
    (EString str1) == (EString str2)  = str1 == str2
    _              == _               = False

export
implementation ToJSON AttributeValue where
  toJSON (AVString str) = toJSON str
  toJSON (AVNumber dbl) = toJSON dbl
  toJSON (AVBool x)     = toJSON x
  toJSON AVNull         = JNull

mutual
  export
  implementation ToJSON Tag where
    toJSON (MkTag name attributes elementList)
      = assert_total -- TODO: Remove assert_total
      $ toJSON
      $ the (List _)
      $ toJSON name :: toJSON attributes :: map toJSON elementList

  export
  implementation ToJSON Element where
    toJSON (ETag tag)    = toJSON tag
    toJSON (EString str) = toJSON str

export
implementation FromJSON AttributeValue where
  fromJSON JNull         = Just AVNull
  fromJSON (JBoolean x)  = Just $ AVBool x
  fromJSON (JNumber dbl) = Just $ AVNumber dbl
  fromJSON (JString str) = Just $ AVString str
  fromJSON _             = Nothing

mutual
  export
  implementation FromJSON Tag where
    fromJSON (JArray ((JString name) :: [])) = Just $ MkTag name empty []
    -- TODO: Remove assert_total
    fromJSON (JArray ((JString name) :: ((JString str) :: xs))) = assert_total $ do
      es <- the (Maybe (List Element)) $ traverse fromJSON xs
      pure $ MkTag name empty es
    -- TODO: Remove assert_total
    fromJSON (JArray ((JString name) :: (x@(JArray _) :: xs))) = assert_total $ do
      es <- the (Maybe (List Element)) $ traverse fromJSON (x :: xs)
      pure $ MkTag name empty es
    -- TODO: Remove assert_total
    fromJSON (JArray ((JString name) :: ((JObject ys) :: xs))) = assert_total $ do
      es <- the (Maybe (List Element)) $ traverse fromJSON xs
      as <- the (Maybe (List (String, AttributeValue)))
          $ traverse (\(n, a) => (n,) <$> fromJSON a) ys
      pure $ MkTag name (fromList as) es
    fromJSON _ = Nothing

  export
  implementation FromJSON Element where
    fromJSON (JString str) = Just $ EString str
    fromJSON json = ETag <$> fromJSON json

export
emptyElement : Element
emptyElement = EString ""

{-
public export
interface ToHTMLString a where
  toHTMLString : a -> String

export
implementation ToHTMLString Element where
  toHTMLString elem = ?todo70

export
implementation ToHTMLString Tag where
  toHTMLString tag = ?todo80

export
implementation ToHTMLString AttributeValue where
  toHTMLString val = ?todo90
-}
