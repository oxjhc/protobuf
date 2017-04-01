{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ProtocolBuffers.Ppr where

import           Data.Int
import           Data.Monoid                   (Last)
import           Data.ProtocolBuffers
import           Data.ProtocolBuffers.Internal
import           Data.Proxy
import           Data.Text                     (Text)
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits
import           Text.PrettyPrint

data Token = Token
  { vnonce   :: Required 1 (Value Text)
  , locn_tag :: Required 2 (Value Text)
  } deriving (Generic, Show, Eq)

class Dataname (f :: * -> *) where
  dataname :: Proxy f -> Doc

instance (Datatype d) => Dataname (D1 d f) where
  dataname _ = text (datatypeName (undefined :: t d f a))

prettyProto :: forall a . (Generic a, GPrettyProto (Rep a)) => Proxy a -> Doc
prettyProto _ = gprettyProto (Proxy :: Proxy (Rep a))

class GPrettyProto (f :: * -> *) where
  gprettyProto :: Proxy f -> Doc

instance (GPrettyProto f, Datatype d) => GPrettyProto (D1 d f) where
  gprettyProto _ = text "message" <+> text (datatypeName (undefined :: t d f a)) <+> lbrace $+$ gprettyProto (Proxy :: Proxy f) $+$ rbrace

instance (GPrettyProto f, Constructor c) => GPrettyProto (C1 c f) where
  gprettyProto _ = nest 2 (gprettyProto (Proxy :: Proxy f))

instance (GPrettyProtoSel f, Selector s) => GPrettyProto (S1 s f) where
  gprettyProto _ = gprettyProtoSel (Proxy :: Proxy f) selName' where
    selName' = selName (undefined :: t s f a)

instance (GPrettyProto x, GPrettyProto y) => GPrettyProto (x :*: y) where
  gprettyProto _ = gprettyProto (Proxy :: Proxy x) $+$ gprettyProto (Proxy :: Proxy y)

class GPrettyProtoSel (f :: * -> *) where
  gprettyProtoSel :: Proxy f -> String -> Doc

instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Required n a)) where
  gprettyProtoSel _ selName' = text "required" <+> protoTypeName (Proxy :: Proxy c) <+> text selName'' <+> text "=" <+> text (show (natVal (Proxy :: Proxy n))) <> char ';' where
    selName'' = if null selName' then "field" ++ show (natVal (Proxy :: Proxy n)) else selName'

instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Optional n a)) where
  gprettyProtoSel _ selName' = text "optional" <+> protoTypeName (Proxy :: Proxy c) <+> text selName'' <+> text "=" <+> text (show (natVal (Proxy :: Proxy n))) <> char ';' where
    selName'' = if null selName' then "field" ++ show (natVal (Proxy :: Proxy n)) else selName'

--
instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Repeated n (Value c))) where
  gprettyProtoSel _ selName' = text "repeated" <+> protoTypeName (Proxy :: Proxy c) <+> text selName'' <+> text "=" <+> text (show (natVal (Proxy :: Proxy n))) <> char ';' where
    selName'' = if null selName' then "field" ++ show (natVal (Proxy :: Proxy n)) else selName'

instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Repeated n (Enumeration c))) where
  gprettyProtoSel _ selName' = text "repeated" <+> protoTypeName (Proxy :: Proxy c) <+> text selName'' <+> text "=" <+> text (show (natVal (Proxy :: Proxy n))) <> char ';' where
    selName'' = if null selName' then "field" ++ show (natVal (Proxy :: Proxy n)) else selName'

instance (KnownNat n, ProtoTypeName c, Dataname (Rep c)) => GPrettyProtoSel (K1 i (Repeated n (Message c))) where
  gprettyProtoSel _ selName' = protoTypeName (Proxy :: Proxy c) $+$ text "repeated" <+> dataname (Proxy :: Proxy (Rep c)) <+> text selName'' <+> text "=" <+> text (show (natVal (Proxy :: Proxy n))) <> char ';' where
    selName'' = if null selName' then "field" ++ show (natVal (Proxy :: Proxy n)) else selName'
--

--
instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Packed n (Value c))) where
  gprettyProtoSel _ selName' = text "repeated" <+> protoTypeName (Proxy :: Proxy c) <+> text selName'' <+> text "=" <+> text (show (natVal (Proxy :: Proxy n))) <+> text "[packed=true]" <> char ';' where
    selName'' = if null selName' then "field" ++ show (natVal (Proxy :: Proxy n)) else selName'

instance (KnownNat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Packed n (Enumeration c))) where
  gprettyProtoSel _ selName' = text "repeated" <+> protoTypeName (Proxy :: Proxy c) <+> text selName'' <+> text "=" <+> text (show (natVal (Proxy :: Proxy n))) <+> text "[packed=true]" <> char ';' where
    selName'' = if null selName' then "field" ++ show (natVal (Proxy :: Proxy n)) else selName'
--

class ProtoTypeName a where
  protoTypeName :: Proxy a -> Doc

instance {-# OVERLAPPABLE #-} ProtoTypeName Int32 where
  protoTypeName _ = text "int32"

instance {-# OVERLAPPABLE #-} ProtoTypeName Int64 where
  protoTypeName _ = text "int64"

instance {-# OVERLAPPABLE #-} ProtoTypeName Word32 where
  protoTypeName _ = text "uint32"

instance {-# OVERLAPPABLE #-} ProtoTypeName Word64 where
  protoTypeName _ = text "uint64"

instance {-# OVERLAPPABLE #-} ProtoTypeName (Fixed Int32) where
  protoTypeName _ = text "sfixed32"

instance {-# OVERLAPPABLE #-} ProtoTypeName (Fixed Int64) where
  protoTypeName _ = text "sfixed64"

instance {-# OVERLAPPABLE #-} ProtoTypeName (Fixed Word32) where
  protoTypeName _ = text "fixed32"

instance {-# OVERLAPPABLE #-} ProtoTypeName (Fixed Word64) where
  protoTypeName _ = text "fixed64"

instance {-# OVERLAPPABLE #-} ProtoTypeName Float where
  protoTypeName _ = text "float"

instance {-# OVERLAPPABLE #-} ProtoTypeName Double where
  protoTypeName _ = text "double"

instance {-# OVERLAPPABLE #-} ProtoTypeName Text where
  protoTypeName _ = text "bytes"

instance {-# OVERLAPS #-} (Generic a, GPrettyProto (Rep a)) => ProtoTypeName a where
  protoTypeName _ = prettyProto (Proxy :: Proxy a)
