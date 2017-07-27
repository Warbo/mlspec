{-# LANGUAGE DeriveGeneric, BangPatterns #-}

module Lib where

import qualified Control.DeepSeq
import qualified Data.Digest.Murmur32 as M32
import qualified Data.List            as DL
import           GHC.Generics
import           GHC.Word
import qualified Data.Serialize       as Ser
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S
import           Weigh

makeMain :: (Control.DeepSeq.NFData a) => String -> (Nat -> a) -> IO ()
makeMain name func = mainWith (checkHash name func)

run h n = h (natExp (fromInt n) (fromInt n))

checkHash name h = sequence (take 4 (hashRuns name h))

hashRuns  name h = [func (name ++ " " ++ show n ++ "^" ++ show n)
                         (run h) n | n <- iterate (*10) 1]

strict :: Nat -> Word32
strict = M32.asWord32 . M32.hash32 . Ser.encode

lazy :: Nat -> Word32
lazy = M32.asWord32 . M32.hash32 . Ser.encodeLazy

strictHash :: Nat -> Word32
strictHash = M32.asWord32 . M32.hash32 . SH . Ser.encodeLazy

natExp x  Z    = S Z
natExp x (S y) = natTimes x (natExp x y)

natTimes  Z    y = Z
natTimes (S x) y = natPlus x (natTimes x y)

natPlus   Z    y = y
natPlus  (S x) y = S (natPlus x y)

fromInt 0 = Z
fromInt n = S (fromInt (abs n - 1))

isEven  Z    = True
isEven (S n) = not (isEven n)

accEven = go True
  where go prev  Z    = prev
        go prev (S n) = go (not prev) n

accBangEven = go True
  where go !prev  Z    = prev
        go !prev (S n) = go (not prev) n

data Nat = Z | S Nat deriving (Generic)

instance Ser.Serialize Nat

instance Control.DeepSeq.NFData Nat

instance Show Nat where
  show = show . Ser.runPut . Ser.put

newtype StrictHashLazyByteString = SH L.ByteString

-- Taken from Data.Digest.Murmur32, but using foldl' instead of foldl

instance M32.Hashable32 StrictHashLazyByteString where
  hash32Add (SH lbs) h = L.foldr go h lbs
    where go :: Word8 -> M32.Hash32 -> M32.Hash32
          go byte hsh = M32.hash32WithSeed (M32.asWord32 hsh)
                                           ((fromIntegral byte :: Word32))
{-
instance M32.Hashable32 StrictHashLazyByteString where
  hash32Add (SH lbs) h = go2 (M32.hash32AddWord32 9) (L.toChunks lbs)
    where go :: (Integral a1) => (t1 -> M32.Hash32) -> a1 -> t1 -> M32.Hash32
          go acc b x    = M32.hash32AddWord32 (fromIntegral b) (acc x)

          go2 :: (M32.Hash32 -> M32.Hash32) -> [S.ByteString] -> M32.Hash32
          go2 !a []     = a h
          go2 !a (c:cs) = go2 (doFoldl a c) cs

          doFoldl :: (t1 -> M32.Hash32) -> S.ByteString -> t1 -> M32.Hash32
          doFoldl z s = DL.foldl' go z (S.unpack s)
-}
