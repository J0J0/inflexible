
maple_show :: (GrevlexPoly Q T) -> String
maple_show (V xs) = intercalate "  +  " (s_coeffmultinom <$> xs)
    where
        s_coeffmultinom (b,q) = "(" ++ show q ++ ") * " ++ s_multinom b
        s_multinom (Grevlex (M _ []))  = "1"
        s_multinom (Grevlex (M _ xis)) = intercalate "*" (s_varpow <$> xis)
        s_varpow (x,1) = s_var x
        s_varpow (x,i) = s_var x ++ "^" ++ show i
        s_var (T k) = "T__" ++ show k

maple_solve :: [GrevlexPoly Q T] -> T -> IO [String]
maple_solve cs (T k) = do
    let ps = intercalate ", " (map maple_show cs)
        i  = "T__" ++ show k
    m <- newMaple
    putMaple m "read \"maple_solve.mpl\":"
    putMaple m $ "solve_constr({" ++ ps ++ "}, " ++ i ++ "):"
    resp <- getMaple m
    closeMaple m
    return resp


