{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, PatternSynonyms, ViewPatterns, FlexibleContexts #-}
module Pure.Data.Prop 
    ( HasProp(..)
    , WithRef(WithRef)
    ) where

import Control.Arrow ((&&&))

-- | Pure.Data.Prop implements an approach to react-style properties that works
-- well with the view patterns in pure-core.  This approach is commonly used for
-- UI frameworks or places where components are highly customizable, but often
-- retain default values for some fields.  In places where the same set of fiels
-- are often initialized together, it can be cleaner and easier to work with a 
-- subcomponent.
--
-- For instance, if you create a set of components that share a `width` 
-- property, you can use `HasProp` along with a custom `Width` pattern to get 
-- and set the width on those components:
--
-- First, create a property proxy.  The constructor commonly uses an underscore
-- to avoid overlapping with the pattern.  This constructor will be in module 
-- that is shared by all of the components with the property.
--
-- > data Width = Width_
--
-- Second, in the same module, write the bidirectional view pattern for the 
-- property.
-- 
-- > pattern Width :: HasProp Width a => Prop Width a -> a -> a
-- > pattern Width p a <- (getProp Width_ &&& id -> (p,a)) where
-- >     Width p a = setProp Width_ p a
--
-- Thid, in the component module, write an instance of `HasProp Width` for your 
-- component.
--
-- > instance HasProp Width MyCompnent1 where
-- >     type Prop Width MyComponent1 = Int
-- >     getProp _ = width
-- >     setProp _ w a = a { width = w }
--
-- Having a default instance for your component can improve markup by making
-- record field instantiation optional. To avoid ambiguity, include an `id` 
-- pattern.
--
-- > instance Default MyComponent1 where def = MyComponent1_ { ... }
-- > pattern MyComponent1 :: MyComponent1 -> MyComponent1
-- > pattern MyComponent1 mc1 = mc1
-- 
-- Then you can use the pure-core patterns along with your default component 
-- pattern and the new Width pattern:
--
-- > MyComponent1 def <| Width 32
--
-- Note that the associated type family allows for different components to use
-- different types for the property, which is especially common in UI 
-- construction, where the same name can be used for different concepts.
--
-- `Pure.Data.Prop.TH` now supports automating the construction of all of the
-- above.

-- | The `HasProp` class links a data type, `p`, to a field, set of fields, or 
-- derived value, `a`. We call a derived value a property and it is generally
-- expected, though not required, that: 
--
-- > setProp SomeProp (getProp SomeProp a) a == a
--
class HasProp p a where
    type Prop p a :: *
    getProp :: p -> a -> Prop p a
    setProp :: p -> Prop p a -> a -> a

-- | A default type for something in the spirit of `withComponentRef`
data WithRef = WithRef_

-- | A property-oriented pattern for accessing a Component's `Ref` from a parent,
-- which generally requires a callback.
pattern WithRef :: HasProp WithRef a => Prop WithRef a -> a -> a
pattern WithRef p a <- (getProp WithRef_ &&& id -> (p,a)) where
     WithRef p a = setProp WithRef_ p a