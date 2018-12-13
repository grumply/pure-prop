{-# LANGUAGE QuasiQuotes, TemplateHaskell, ViewPatterns #-}
module Pure.Data.Prop.TH 
    ( -- * Low-level derivers; often used in libraries to manage property reuse
      mkProp
    , mkPropMany
    , mkHas
    , mkHasMany
      -- * Medium-level deriver
    , mkLocalProps
    , mkComponentPattern
      -- * High-level derivers
    , deriveLocalProps
    , deriveHasChildren
    , deriveHasFeatures
      -- * Batteries-included deriver; often used in standalone libraries
    , deriveLocalComponent
      -- * Included exports
    , (&&&)
    ) where

import Pure.Data.View.Patterns
import Pure.Data.Prop

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Arrow ((&&&))
import Data.Char
import Data.Traversable (traverse)
import Data.List

-- | Given a property name, prop constructs a property type and a property 
-- pattern.
--
-- > mkProp "Label"
-- 
-- Produces:
--
-- > data Label = Label_
-- > pattern Label :: HasProp Label a => Prop Label a -> a -> a
-- > pattern Label p a <- (getProp Label_ &&& id -> (p,a)) where
-- >     Label p a = setProp Label_ p a
--
mkProp :: String -> Q [Dec]
mkProp nm = do
    Just _and <- lookupValueName "&&&"
    let p   = mkName "p"
        a   = mkName "a"
        n   = mkName nm
        n_  = mkName (nm ++ "_")

        dat = DataD [] n [] Nothing [NormalC n_ []] []

        typ = 
            let tyv = [PlainTV a]
                cxt = [(ConT ''HasProp `AppT` ConT n) `AppT` VarT a]
                pty = (ConT ''Prop `AppT` ConT n) `AppT` VarT a
                aty = (ArrowT `AppT` pty) `AppT` ((ArrowT `AppT` VarT a) `AppT` VarT a)
            in PatSynSigD n $ ForallT tyv cxt aty
            
        bid =
            let exp = ((VarE 'setProp `AppE` ConE n_) `AppE` VarE p) `AppE` VarE a
                bnd = [VarP p,VarP a]
                cls = [Clause bnd (NormalB exp) []]
            in ExplBidir cls

        pat = 
            let get = InfixE (Just (VarE 'getProp `AppE` ConE n_)) (VarE _and) (Just (VarE 'id))
                pat = TupP [VarP p,VarP a]
            in ViewP get pat

        dec = 
            let pre = PrefixPatSyn [p,a]
            in PatSynD n pre bid pat

    return [dat,typ,dec]

-- We cannot use pure-txt-interpolate because haskell-src-meta doesn't 
-- support pattern synonyms
--
-- [i|data #{nm} = #{nm}_
-- pattern #{nm} :: HasProp #{nm} a => Prop #{nm} a -> a -> a
-- pattern #{nm} p a <- (getProp #{nm}_ &&& id -> (p,a)) where
--     #{nm} p a = setProp #{nm}_ p a
-- |]

-- | Given a list of property names, produce property types and patterns
-- for each.
mkPropMany :: [String] -> Q [Dec]
mkPropMany = fmap concat . traverse mkProp

-- | Given a component name, a property name, and a property accessor name, 
-- `has` constructs an instancce of `HasProp` for that property. 
--
-- > mkHas ''SomeComponent ''Size 'size
--
-- Produces:
--
-- > instance HasProp Size SomeComponent where
-- >     type Prop Size SomeComponent = <<result type of the size accessor>>
-- >     getProp _ = size
-- >     setProp _ x a = a { size = x }
--
mkHas :: Name -> Name -> Name -> Q [Dec]
mkHas comp prop labl = do
    TyConI (DataD _ nm tyvs _ _ _) <- reify comp
    VarI _ (fieldResult -> acc) _ <- reify labl -- 
    let p = mkName "p"
        a = mkName "a"

        cty = ((ConT ''HasProp) `AppT` ConT prop) `AppT` (datType nm tyvs)

        pty = 
            let tse = TySynEqn [ConT prop,datType nm tyvs] acc
            in TySynInstD ''Prop tse 

        get = 
            let bdy = VarE labl
                cls = [Clause [WildP] (NormalB bdy) []]
            in FunD 'getProp cls 

        set = 
            let bdy = RecUpdE (VarE a) [(labl,VarE p)]
                cls = [Clause [WildP,VarP p,VarP a] (NormalB bdy) []]
            in FunD 'setProp cls

        ins = InstanceD Nothing [] cty [pty,get,set]

    return [ins]
    where
        datType :: Name -> [TyVarBndr] -> Type
        datType nm = foldl AppT (ConT nm) . fmap toType
            where
                toType (PlainTV nm) = VarT nm
                toType (KindedTV nm _) = VarT nm

        fieldResult :: Type -> Type
        fieldResult (ForallT _ _ ty) = fieldResult ty
        fieldResult (AppT (AppT ArrowT _) ty) = ty
        fieldResult ty = error ("incomprehensible accessor: " ++ show ty)

-- We cannot use pure-txt-interpolate because haskell-src-meta doesn't 
-- support associated type families.
--
--     [i|instance HasProp #{nm} #{ty} where
--     type Prop #{nm} #{ty} = #{pty}
--     getProp _ = #{acc}
--     setProp _ x a = a { #{acc} = x }
-- |]

-- | Given a component name and a zipped list of property type names and 
-- field accessor names, constructs instances of `HasProp` for each property 
-- and accessor pair.
mkHasMany :: Name -> [(Name,Name)] -> Q [Dec]
mkHasMany comp = fmap concat . traverse (uncurry (mkHas comp))

-- | Given a name and list of property type names zipped with field 
-- accessor names, constructs properties for each property type and
-- instances of `HasProp` for each property and accessor pair.
--
-- For utility libraries, this is a common approach for property 
-- implementation. For shared libraries, it is common to split
-- the `prop` generation from the `has` generation for more 
-- consistent modularization.
--
-- See `mkProp` to learn about property type construction.
-- See `mkHas` to learn about `HasProp` instantiation.
mkLocalProps :: Name -> [(String,Name)] -> Q [Dec]
mkLocalProps comp nls = do
    ds <- mkPropMany (fmap fst nls)
    is <- mkHasMany comp $ fmap (\(n,l) -> (mkName n,l)) nls
    return (ds ++ is)

-- | Given a name, derive a type-fixing pattern of the same name. To use this 
-- method, the component is required to have a constructor with a different 
-- name from the one supplied here.
mkComponentPattern :: Name -> Q [Dec]
mkComponentPattern comp@(Name (OccName occ) _) = do
    TyConI (DataD _ nm tyvs _ _ _) <- reify comp
    let c = mkName "c"
        nm = mkName occ
        ty = datType nm tyvs
        typ = PatSynSigD nm (ForallT [] [] (AppT (AppT ArrowT ty) ty))
        dec = PatSynD nm (PrefixPatSyn [c]) ImplBidir (VarP c)
    return [typ,dec]

-- | Automatically derive properties for non-feature and non-children features
-- for the given component name. These are local props because the property 
-- types are defined locally, which prevents the modularization often expected 
-- in libraries.
deriveLocalProps :: Name -> Q [Dec]
deriveLocalProps nm = do
    TyConI (DataD _ _ _ _ [RecC _ fs] _) <- reify nm
    let capitalize [] = []
        capitalize (c:cs) = toUpper c : cs
        cls = fmap (\((nm@(Name (OccName occ) _),_,_)) -> (capitalize occ,nm)) fs
        noFeatures = filter (\(x,_) -> x /= "Features") 
        noChildren = filter (\(x,_) -> x /= "Children")
    mkLocalProps nm (noChildren (noFeatures cls))

-- | Automatically derive an instance of `HasChildren` for the given component
-- name. Uses the first field for which the name is `children` or the type is
-- `[View]`. If the component has no such field, it simply returns an empty 
-- list.
deriveHasChildren :: Name -> Q [Dec]
deriveHasChildren nm = do
    TyConI (DataD _ _ tyvs _ [RecC _ fs] _) <- reify nm
    let lts = fmap (\(Name (OccName occ) _,_,ty) -> (occ,ty)) fs
        vty = ListT `AppT` (ConT (mkName "View"))
        children = filter (\(occ,ty) -> ty == vty || occ == "children") lts
    case children of
        [] -> return []
        ((mkName -> occ,_):_) -> do
            let a  = mkName "a"
                cs = mkName "cs"

                ity = ((ConT ''HasChildren) `AppT` datType nm tyvs)

                get = 
                    let cls = [Clause [] (NormalB (VarE occ)) []]
                    in FunD 'getChildren cls

                set =
                    let bdy = RecUpdE (VarE a) [(occ,VarE cs)]
                        cls = [Clause [VarP cs, VarP a] (NormalB bdy) []]
                    in FunD 'setChildren cls

                ins = InstanceD Nothing [] ity [get,set]

            return [ins]

datType :: Name -> [TyVarBndr] -> Type
datType nm = foldl AppT (ConT nm) . fmap toType
    where
        toType (PlainTV nm) = VarT nm
        toType (KindedTV nm _) = VarT nm 

-- | Automatically derive an instance of `HasFeatures` for the given component 
-- name. Uses the first field for which the name is `features` or the type is
-- `Features`. If the component has no such field, it simply returns an empty 
-- list.
deriveHasFeatures :: Name -> Q [Dec]
deriveHasFeatures nm = do
    TyConI (DataD _ _ tyvs _ [RecC _ fs] _) <- reify nm
    let lts = fmap (\(Name (OccName occ) _,_,ty) -> (occ,ty)) fs
        vty = ConT (mkName "Features")
        features = filter (\(occ,ty) -> ty == vty || occ == "features") lts
    case features of
        [] -> return []
        ((mkName -> occ,_):_) -> do
            let a  = mkName "a"
                fs = mkName "fs"

                ity = ((ConT ''HasFeatures) `AppT` datType nm tyvs)

                get = 
                    let cls = [Clause [] (NormalB (VarE occ)) []]
                    in FunD 'getFeatures cls

                set =
                    let bdy = RecUpdE (VarE a) [(occ,VarE fs)]
                        cls = [Clause [VarP fs, VarP a] (NormalB bdy) []]
                    in FunD 'setFeatures cls

                ins = InstanceD Nothing [] ity [get,set]

            return [ins]

-- | Batteries-included deriver. Given a component name, constructs a 
-- type-fixing pattern via `mkComponentPattern`, an instance of `HasChildren`
-- via `deriveHasChildren` if applicable, an instance of `HasFeatures` via
-- `deriveHasFeatures` if applicable, and local properties for all other
-- fields via `deriveLocalProps`.
deriveLocalComponent :: Name -> Q [Dec]
deriveLocalComponent nm = do
    p  <- mkComponentPattern nm
    cs <- deriveHasChildren nm
    fs <- deriveHasFeatures nm
    ps <- deriveLocalProps nm
    return (p ++ cs ++ fs ++ ps)

