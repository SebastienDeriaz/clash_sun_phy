module SunPhy.AXI where

import Clash.Prelude

data AxiForward a = AxiForward
  { valid :: Bit
  , _data :: a
  , last :: Bit
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

data AxiBackward = AxiBackward
  { ready :: Bit
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)