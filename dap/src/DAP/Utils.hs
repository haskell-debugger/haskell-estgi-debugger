{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
----------------------------------------------------------------------------
-- |
-- Module      :  DAP.Utils
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module DAP.Utils where
----------------------------------------------------------------------------
import           GHC.Generics               (Generic, Rep)
import           Data.Aeson                 ( ToJSON(toJSON), Value, fieldLabelModifier
                                            , genericToJSON, genericParseJSON, fieldLabelModifier
                                            , defaultOptions, GToJSON, GFromJSON, Zero, Options
                                            , constructorTagModifier, sumEncoding, SumEncoding(UntaggedValue), omitNothingFields
                                            )
import           Data.Aeson.Types           ( Parser )
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import           Data.ByteString            ( ByteString )
import           Data.Char                  ( isLower, toLower, toUpper )
import           Data.Proxy                 (Proxy(Proxy))
import           Data.Typeable              ( Typeable, typeRep )
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8      as BS
----------------------------------------------------------------------------
-- | Encodes DAP protocol message appropriately
-- >
encodeBaseProtocolMessage :: ToJSON a => a -> ByteString
encodeBaseProtocolMessage msg =
  mconcat
  [ "Content-Length: " <> BS.pack (show (BS.length bytes)) <> "\r\n\r\n"
  , bytes
  ] where
      bytes = BL8.toStrict (encodePretty msg)
----------------------------------------------------------------------------
-- | Capitalization helper function
-- >>> capitalize "fooBar"
-- >>> "FooBar"
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs)
  | isLower x = toUpper x : xs
  | otherwise = x : xs
----------------------------------------------------------------------------
-- | Lower cases a word
-- >>> toLowerCase "FooBar"
-- >>> "fooBar"
toLowerCase :: String -> String
toLowerCase [] = []
toLowerCase (x:xs) = toLower x : xs
----------------------------------------------------------------------------
-- | Used as a fieldLabelModifier when generating aeson parsers
-- >>> modifier (Proxy @Int) "intThing"
-- >>> String "thing"
modifier
  :: Typeable a
  => proxy a
  -> String
  -> String
modifier proxy
  = toLowerCase
  . drop (length (getName proxy))
----------------------------------------------------------------------------
-- | Used as a fieldLabelModifier when generating aeson parsers
-- >>> getName (Proxy @Int)
-- >>> "Int"
getName
  :: Typeable a
  => proxy a
  -> String
getName proxy = show (typeRep proxy)
----------------------------------------------------------------------------
-- | Used as a fieldLabelModifier when generating aeson parsers
-- >>> getName (Proxy @Int)
-- >>> "Int"
genericToJSONWithModifier
  :: forall a . (Generic a, GToJSON Zero (Rep a), Typeable a)
  => a -> Value
genericToJSONWithModifier
  = genericToJSON defaultOptions
  { fieldLabelModifier = modifier (Proxy @a)
  , constructorTagModifier = modifier (Proxy @a)
  , sumEncoding = UntaggedValue
  , omitNothingFields = True
  }
----------------------------------------------------------------------------
-- | Used as a fieldLabelModifier when generating aeson parsers
-- >>> getName (Proxy @Int)
-- >>> "Int"
genericParseJSONWithModifier
  :: forall a . (Generic a, GFromJSON Zero (Rep a), Typeable a)
  => Value
  -> Parser a
genericParseJSONWithModifier
  = genericParseJSON defaultOptions
  { fieldLabelModifier = modifier (Proxy @a)
  , constructorTagModifier = modifier (Proxy @a)
  , sumEncoding = UntaggedValue
  , omitNothingFields = True
  }
----------------------------------------------------------------------------
-- | Log formatting util
withBraces :: BL8.ByteString -> BL8.ByteString
withBraces x  = "[" <> x <> "]"
----------------------------------------------------------------------------
