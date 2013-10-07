{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

import Foreign
import Foreign.C

#include "foo.h"

data Bar = Bar { a::Int, b::Int }
type BarPtr = Ptr Bar

foreign import ccall "static foo.h print_foo"
  f_print_foo :: BarPtr -> IO()

instance Storable Bar where
  sizeOf _ = (#size Foo)
  alignment _ = alignment (undefined :: CInt)
  peek _ = error "none"
  poke ptr (Bar a' b') = do
    (#poke Foo, a) ptr a'
    (#poke Foo, b) ptr b'












