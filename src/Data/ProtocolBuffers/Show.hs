{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.ProtocolBuffers.Show where

import           Data.Int
import           Data.Monoid                   (Last)
import           Data.ProtocolBuffers.Internal
import           Data.ProtocolBuffers.Schema
import           Data.ProtocolBuffers.Types
import           Data.Proxy
import           Data.Text                     (Text)
import           Data.Type.Bool
import           Data.Word
import           Debug.Trace
import           GHC.Generics
import           GHC.TypeLits
import           Text.PrettyPrint

data Single = Single (Required 1 (Value Word32)) (Required 2 (Value Word32))
  deriving (Generic, Show, Eq)

data Point = Point
  { x :: Required 1 (Value (Signed Int32))
  , y :: Packed 2 (Value Int32)
  } deriving (Generic, Show, Eq)

data Vault = Vault
  { points :: Optional 1 (Message Point)
  } deriving (Generic, Show, Eq)

data VaultMsg = VaultMsg
  { vault   :: Required 1 (Message Vault)
  , uid     :: Required 2 (Value Text)
  , unonce  :: Required 3 (Value Text)
  , apid    :: Required 4 (Value Text)
  , apnonce :: Required 5 (Value Text)
  , time    :: Required 6 (Value (Fixed Word64))
  , sig     :: Required 7 (Value Text)
  } deriving (Generic, Show, Eq)

showProto :: forall a . (Generic a, GShowProto (Rep a)) => a -> Doc
showProto = gshowProto . from

class GShowProto (f :: * -> *) where
  gshowProto :: f a -> Doc

instance (GShowProto f, Datatype d) => GShowProto (D1 d f) where
  gshowProto (M1 x) = dataname @(D1 d f) <+> braces (gshowProto x)

instance (GShowProto f, Constructor c) => GShowProto (C1 c f) where
  gshowProto (M1 x) = gshowProto x

instance (GShowProtoSel f, Selector s) => GShowProto (S1 s f) where
  gshowProto (M1 x) = gshowProtoSel selName' x where
    selName' = selName (undefined :: t s f a)

instance (GShowProto a, GShowProto b) => GShowProto (a :*: b) where
  gshowProto (x :*: y) = gshowProto x <> char ',' <+> gshowProto y

class GShowProtoSel (f :: * -> *) where
  gshowProtoSel :: String -> f a -> Doc

instance (KnownNat n, ProtoShow (Required n c)) => GShowProtoSel (K1 i (Required n c)) where
  gshowProtoSel selName (K1 x) = text (makeSelName @n selName) <+> text "=" <+> protoShow x

instance (KnownNat n, ProtoShow (Optional n c)) => GShowProtoSel (K1 i (Optional n c)) where
  gshowProtoSel selName (K1 x) = text (makeSelName @n selName) <+> text "=" <+> protoShow x

instance (KnownNat n, ProtoShow (Repeated n c)) => GShowProtoSel (K1 i (Repeated n c)) where
  gshowProtoSel selName (K1 x) = text (makeSelName @n selName) <+> text "=" <+> protoShow x

instance (KnownNat n, ProtoShow (Packed n c)) => GShowProtoSel (K1 i (Packed n c)) where
  gshowProtoSel selName (K1 x) = text (makeSelName @n selName) <+> text "=" <+> protoShow x

class ProtoShow a where
  protoShow :: a -> Doc

instance (ProtoShow' Top Mid a) => ProtoShow a where
  protoShow = protoShow' (Proxy @Top) (Proxy @Mid)

type family IsBase a :: Bool where
  IsBase Double = True
  IsBase Float = True
  IsBase Int32 = True
  IsBase Int64 = True
  IsBase Word32 = True
  IsBase Word64 = True
  IsBase (Signed Int32) = True
  IsBase (Signed Int64) = True
  IsBase (Fixed Int32) = True
  IsBase (Fixed Int64) = True
  IsBase (Fixed Word32) = True
  IsBase (Fixed Word64) = True
  IsBase Bool = True
  IsBase Text = True
  IsBase _ = False

data LevelTag = Bottom | Mid | Top | Fail

type family CurrentLevel a :: LevelTag where
  CurrentLevel (Required n (f a)) = Mid
  CurrentLevel (Optional n (f a)) = Mid
  CurrentLevel (Repeated n (f a)) = Mid
  CurrentLevel (Packed n (f a)) = Mid
  CurrentLevel (Value a) = Mid
  CurrentLevel (Enumeration a) = Mid
  CurrentLevel (Message a) = Mid
  CurrentLevel (Maybe a) = Mid
  CurrentLevel [a] = Mid
  CurrentLevel a = If (IsBase a) Bottom Top

type family NextLevel a :: LevelTag where
  NextLevel (Required n (f a)) = NextLevel (f a)
  NextLevel (Optional n (f a)) = Mid
  NextLevel (Repeated n (f a)) = Mid
  NextLevel (Packed n (f a)) = Mid
  NextLevel (Value a) = CurrentLevel a
  NextLevel (Enumeration a) = CurrentLevel a
  NextLevel (Message a) = Top
  NextLevel (Maybe a) = CurrentLevel a
  NextLevel [a] = CurrentLevel a
  NextLevel a = If (IsBase a) Bottom Fail

class ProtoShow' (prev :: LevelTag) (cur :: LevelTag) a where
  protoShow' :: Proxy prev -> Proxy cur -> a -> Doc

instance (Generic a, GShowProto (Rep a)) => ProtoShow' prev Top a where
  protoShow' _ _ = showProto

instance (KnownNat n,
          next ~ NextLevel (Required n (f a)),
          HasField (Required n (f a)),
          ProtoShow' Mid next (FieldType (Required n (f a))))
          => ProtoShow' Top Mid (Required n (f a)) where
  protoShow' _ _ = protoShow' (Proxy @Mid) (Proxy @next) . getField

instance (KnownNat n,
          next ~ NextLevel (Optional n (f a)),
          HasField (Optional n (f a)),
          ProtoShow' Mid next (FieldType (Optional n (f a))))
          => ProtoShow' Top Mid (Optional n (f a)) where
  protoShow' _ _ = protoShow' (Proxy @Mid) (Proxy @next) . getField

instance (KnownNat n,
          next ~ NextLevel (Field n (f a)),
          HasField (Field n (f a)),
          ProtoShow' Mid next (FieldType (Field n (f a))))
          => ProtoShow' Top Mid (Field n (f a)) where
  protoShow' _ _ = protoShow' (Proxy @Mid) (Proxy @next) . getField

instance (Functor f, next ~ NextLevel (f a), ProtoShow' Mid next a, Show (f Doc)) => ProtoShow' Mid Mid (f a) where
  protoShow' _ _ = text . show . fmap (protoShow' (Proxy @Mid) (Proxy @next))

instance Show a => ProtoShow' Mid Bottom a where
  protoShow' _ _ = text . show
