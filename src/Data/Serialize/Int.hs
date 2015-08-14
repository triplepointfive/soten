{- A batch of helpers for serializing Int values -}
module Data.Serialize.Int where

import           Data.Bits
import           Data.Int

import qualified Data.ByteString.Unsafe as B
import           Data.Serialize.Get

-- | Read a Int8 from the monad state
getInt8 :: Get Int8
getInt8 = do
    s <- getBytes 1
    return $! fromIntegral (B.unsafeHead s)

-- | Read a Int16 in big endian format
getInt16be :: Get Int16
getInt16be = do
    s <- getBytes 2
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftL` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 1) )

-- | Read a Int16 in little endian format
getInt16le :: Get Int16
getInt16le = do
    s <- getBytes 2
    return $! (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

-- | Read a Int32 in big endian format
getInt32be :: Get Int32
getInt32be = do
    s <- getBytes 4
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )

-- | Read a Int32 in little endian format
getInt32le :: Get Int32
getInt32le = do
    s <- getBytes 4
    return $! (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

-- | Read a Int64 in big endian format
getInt64be :: Get Int64
getInt64be = do
    s <- getBytes 8
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftL` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )

-- | Read a Int64 in little endian format
getInt64le :: Get Int64
getInt64le = do
    s <- getBytes 8
    return $! (fromIntegral (s `B.unsafeIndex` 7) `shiftL` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftL` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftL` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftL` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

