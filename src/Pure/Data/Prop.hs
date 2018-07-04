{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Pure.Data.Prop where

class HasProp p a where
    type Prop p a :: *
    getProp :: p -> a -> Prop p a
    setProp :: p -> Prop p a -> a -> a
