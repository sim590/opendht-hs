
module OpenDHT.Internal.Socket where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

#include <Socket.h>

data CSockAddr = CSockAddr { _sa_family :: CUShort
                           , _sa_addr   :: Ptr CChar
                           }
               | CSockAddrIn { _sin_family :: CUShort
                             , _sin_port   :: CUShort
                             , _sin_addr   :: Ptr ()
                             , _sin_zero   :: Ptr CUChar
                             }
               | CSockAddrIn6 { _sin6_family   :: CUShort
                              , _sin6_port     :: CUShort
                              , _sin6_flowinfo :: CUInt
                              , _sin6_addr     :: Ptr ()
                              , _sin6_scope_id :: CUInt
                              }


instance Storable CSockAddr where
    sizeOf _    = {# sizeof  td_sock_addr #}
    alignment _ = {# alignof td_sock_addr #}
    poke        = poke
    peek p      = do
      sa_family <- {# get td_sock_addr->sa_family #} p
      case sa_family of
        -- AF_INET
        2  -> CSockAddrIn <$> {# get td_sock_addr_in->sin_family #} p
                          <*> {# get td_sock_addr_in->sin_port   #} p
                          <*> {# get td_sock_addr_in->sin_addr   #} p
                          <*> {# get td_sock_addr_in->sin_zero   #} p
        -- AF_INET6
        10 -> CSockAddrIn6 <$> {# get td_sock_addr_in6->sin6_family   #} p
                           <*> {# get td_sock_addr_in6->sin6_port     #} p
                           <*> {# get td_sock_addr_in6->sin6_flowinfo #} p
                           <*> {# get td_sock_addr_in6->sin6_addr     #} p
                           <*> {# get td_sock_addr_in6->sin6_scope_id #} p
        -- unsupported
        _  -> CSockAddr <$> {# get td_sock_addr->sa_family #} p
                        <*> {# get td_sock_addr->sa_data   #} p

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

