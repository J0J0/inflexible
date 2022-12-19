
-- volume forms given by monomials
vols = map generify $ dropCache $ do
    let dim = formalDimension my_dga
    ms <- findBasis my_dga dim
    filterM is_vol_form ms
  where
    generify :: Num k => Lam Q G -> Lam k G
    generify = pure . head . basisElems
    is_vol_form m = do
        is_bdy <- isCoboundary my_dgaQ m
        return $ isCocycle' my_dgaQ m && not is_bdy

-- volume forms that are even given by a generator power
gen_power_vols = filter check_single_gen vol_bs
  where
    check_single_gen :: Eq a => FGCA a -> Bool
    check_single_gen (Sym _ xs, Ext _ ys) =  1 == length (group (xs <> ys))
