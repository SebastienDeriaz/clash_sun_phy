module SunPhy.Settings where

data AxiForward = AxiForward
    { _valid :: Bit
    , _data :: Bit
    , _last :: Bit
    }

data AxiBackward = AxiBackward
    { _ready :: Bit
    }