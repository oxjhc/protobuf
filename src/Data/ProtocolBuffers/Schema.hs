{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ProtocolBuffers.Schema
  ( schemaProto
  , Dataname(..)
  , makeSelName
  ) where

import           Data.Int
import           Data.Monoid                   (Last)
import           Data.ProtocolBuffers.Internal
import           Data.ProtocolBuffers.Types
import           Data.Proxy
import           Data.Text                     (Text)
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits
import           Text.PrettyPrint

class Dataname (f :: * -> *) where
  dataname :: Doc

instance (Datatype d) => Dataname (D1 d f) where
  dataname = text (datatypeName (undefined :: t d f a))

schemaProto :: forall a . (Generic a, GPrettyProto (Rep a)) => Doc
schemaProto = gprettyProto @(Rep a)

class GPrettyProto (f :: * -> *) where
  gprettyProto :: Doc

instance (GPrettyProto f, Datatype d) => GPrettyProto (D1 d f) where
  gprettyProto =
    text "message" <+>
    dataname @(D1 d f) <+>
    lbrace $+$ gprettyProto @f $+$ rbrace

instance (GPrettyProto f, Constructor c) => GPrettyProto (C1 c f) where
  gprettyProto = nest 2 (gprettyProto @f)

instance (GPrettyProtoSel f, Selector s) => GPrettyProto (S1 s f) where
  gprettyProto = gprettyProtoSel @f selName' where
    selName' = selName (undefined :: t s f a)

instance (GPrettyProto x, GPrettyProto y) => GPrettyProto (x :*: y) where
  gprettyProto = gprettyProto @x $+$ gprettyProto @y

class GPrettyProtoSel (f :: * -> *) where
  gprettyProtoSel :: String -> Doc

makeSelName :: forall n. KnownNat n => String -> String
makeSelName s = if null s then "field" ++ show (natVal (Proxy @n)) else s

makeNat :: forall n. KnownNat n => String
makeNat = show (natVal (Proxy @n))

instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Required n c)) where
  gprettyProtoSel selName =
    text "required" <+> protoTypeName @c <+> text (makeSelName @n selName) <+>
    text "=" <+> text (makeNat @n) <> char ';'

instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Optional n c)) where
  gprettyProtoSel selName =
    text "optional" <+> protoTypeName @c <+> text (makeSelName @n selName) <+>
    text "=" <+> text (makeNat @n) <> char ';'

instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Repeated n c)) where
  gprettyProtoSel selName =
    text "required" <+> protoTypeName @c <+> text (makeSelName @n selName) <+>
    text "=" <+> text (makeNat @n) <> char ';'

instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Packed n c)) where
  gprettyProtoSel selName =
    text "repeated" <+> protoTypeName @c <+> text (makeSelName @n selName) <+>
    text "=" <+> text (makeNat @n) <+> text "[packed=true]" <> char ';'

class ProtoTypeName a where
  protoTypeName :: Doc

instance ProtoTypeName Double where
  protoTypeName = text "double"

instance ProtoTypeName Float where
  protoTypeName = text "float"

instance ProtoTypeName Int32 where
  protoTypeName = text "int32"

instance ProtoTypeName Int64 where
  protoTypeName = text "int64"

instance ProtoTypeName Word32 where
  protoTypeName = text "uint32"

instance ProtoTypeName Word64 where
  protoTypeName = text "uint64"

instance ProtoTypeName (Signed Int32) where
  protoTypeName = text "sint32"

instance ProtoTypeName (Signed Int64) where
    protoTypeName = text "sint64"

instance ProtoTypeName (Fixed Int32) where
  protoTypeName = text "sfixed32"

instance ProtoTypeName (Fixed Int64) where
  protoTypeName = text "sfixed64"

instance ProtoTypeName (Fixed Word32) where
  protoTypeName = text "fixed32"

instance ProtoTypeName (Fixed Word64) where
  protoTypeName = text "fixed64"

instance ProtoTypeName Bool where
  protoTypeName = text "bool"

instance ProtoTypeName Text where
  protoTypeName = text "bytes"

instance ProtoTypeName a => ProtoTypeName (Value a) where
  protoTypeName = protoTypeName @a

instance ProtoTypeName a => ProtoTypeName (Enumeration a) where
  protoTypeName = protoTypeName @a

instance Dataname (Rep a) => ProtoTypeName (Message a) where
  protoTypeName = dataname @(Rep a)
