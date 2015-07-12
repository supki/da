{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Convenient module that simplifies lives of damit users greatly
module Damit.Route.Extra
  ( -- * Inference helpers
    string
  , integer
  , int
  , int64
  , int32
  , int16
  , int8
  , word
  , word64
  , word32
  , word16
  , word8
  , double
  , float
  ) where

import Data.Int (Int64, Int32, Int16, Int8)
import Data.Word (Word64, Word32, Word16, Word8)
#if __GLASGOW_HASKELL__ < 710
import Data.Word (Word)
#endif

import Damit


-- | Declare the next input piece a 'String'
string :: Arg s String => Pattern s String
string = bind

-- | Declare the next input piece an 'Integer'
integer :: Arg s Integer => Pattern s Integer
integer = bind

-- | Declare the next input piece an 'Int'
int :: Arg s Int => Pattern s Int
int = bind

-- | Declare the next input piece an 'Int64'
int64 :: Arg s Int64 => Pattern s Int64
int64 = bind

-- | Declare the next input piece an 'Int32'
int32 :: Arg s Int32 => Pattern s Int32
int32 = bind

-- | Declare the next input piece an 'Int16'
int16 :: Arg s Int16 => Pattern s Int16
int16 = bind

-- | Declare the next input piece an 'Int8'
int8 :: Arg s Int8 => Pattern s Int8
int8 = bind

-- | Declare the next input piece a 'Word'
word :: Arg s Word => Pattern s Word
word = bind

-- | Declare the next input piece a 'Word64'
word64 :: Arg s Word64 => Pattern s Word64
word64 = bind

-- | Declare the next input piece a 'Word32'
word32 :: Arg s Word32 => Pattern s Word32
word32 = bind

-- | Declare the next input piece a 'Word16'
word16 :: Arg s Word16 => Pattern s Word16
word16 = bind

-- | Declare the next input piece a 'Word8'
word8 :: Arg s Word8 => Pattern s Word8
word8 = bind

-- | Declare the next input piece a 'Double'
double :: Arg s Double => Pattern s Double
double = bind

-- | Declare the next input piece a 'Float'
float :: Arg s Float => Pattern s Float
float = bind
