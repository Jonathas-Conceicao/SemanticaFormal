import Estado
import Tipo

interpretA :: (Exp, Estado) -> (Exp, Estado)
interpretA (a, s) = if isFinal a then (a, s) else interpretA (aSmallStep (a, s))

aSmallStep :: (Exp, Estado) -> (Exp, Estado)
aSmallStep (Var x, s)               = (Num (procuraVar s x), s)
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y), s)
aSmallStep (Som (Num x) e2, s)      = (Som (Num x) ef, s) where (ef, _) = aSmallStep (e2, s)
aSmallStep (Som e1 e2, s)           = (Som ef e2, s) where (ef, _) = aSmallStep (e1, s)
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y), s)
aSmallStep (Sub (Num x) e2, s)      = (Sub (Num x) ef, s) where (ef, _) = aSmallStep (e2, s)
aSmallStep (Sub e1 e2, s)           = (Sub ef e2, s) where (ef, _) = aSmallStep (e1, s)
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y), s)
aSmallStep (Mul (Num x) e2, s)      = (Mul (Num x) ef, s) where (ef, _) = aSmallStep (e2, s)
aSmallStep (Mul e1 e2, s)           = (Mul ef e2, s) where (ef, _) = aSmallStep (e1, s)

interpretB :: (Exp,Estado) -> (Exp,Estado)
interpretB (b, s) = if isFinal b then (b, s) else interpretB (bSmallStep (b, s))

bSmallStep :: (Exp,Estado) -> (Exp,Estado)
bSmallStep (Not FALSE, s)           = (TRUE, s)
bSmallStep (Not TRUE,  s)           = (FALSE, s)
bSmallStep (Not b, s)               = (Not bn, s) where (bn, _) = bSmallStep (b, s)
bSmallStep (And TRUE  b2, s)        = (b2, s)
bSmallStep (And FALSE b2, s)        = (FALSE, s)
bSmallStep (And b1 b2, s)           = (And bn b2, s) where (bn, _) = bSmallStep (b1, s)
bSmallStep (Or FALSE b2, s)         = (b2, s)
bSmallStep (Or TRUE  b2, s)         = (TRUE, s)
bSmallStep (Or b1 b2, s)            = (Or  bn b2, s) where (bn, _) = bSmallStep (b1, s)
bSmallStep (Ig (Num x) (Num y), s)  = (if x == y then TRUE else FALSE, s)
bSmallStep (Ig (Num x) b, s)        = (Ig (Num x) bn, s) where (bn, _) = aSmallStep (b, s)
bSmallStep (Ig b1      b2, s)       = (Ig bn b2, s) where (bn, _) = aSmallStep (b1, s)
bSmallStep (Leq (Num x) (Num y), s) = (if x <= y then TRUE else FALSE, s)
bSmallStep (Leq (Num x) b, s)       = (Leq (Num x) bn, s) where (bn, _) = aSmallStep (b, s)
bSmallStep (Leq b1      b2, s)      = (Leq bn b2, s) where (bn, _) = aSmallStep (b1, s)

interpretC :: (Exp, Estado) -> (Exp, Estado)
interpretC (c, s) = if isFinal c then (c, s) else interpretC (cSmallStep (c, s))

cSmallStep :: (Exp, Estado) -> (Exp, Estado)
cSmallStep (Atrib (Var v) (Num x), s) = (Skip, ns) where ns = mudaVar s v x
cSmallStep (Atrib (Var v) a, s)       = (Atrib (Var v) an, s) where (an, _) = aSmallStep (a, s)
cSmallStep (Seq Skip  c,  s)          = (c, s)
cSmallStep (Seq Throw c,  s)          = (Throw, s)
cSmallStep (Seq c1    c2, s)          = (Seq cn c2, sn) where (cn, sn) = cSmallStep (c1, s)
cSmallStep (If TRUE  c1 c2, s)        = (c1, s)
cSmallStep (If FALSE c1 c2, s)        = (c2, s)
cSmallStep (If b     c1 c2, s)        = (If bn c1 c2, s) where (bn, _) = bSmallStep (b, s)
cSmallStep (Catch Skip  c,  s)        = (Skip, s)
cSmallStep (Catch Throw c,  s)        = (c, s)
cSmallStep (Catch c1    c2, s)        = (Catch cn c2, sn) where (cn, sn) = cSmallStep (c1, s)
cSmallStep (While b c, s)             = (If b (Seq c (While b c)) (Skip), s)

meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

exemploE1 :: Exp
exemploE1 = (While (Leq (Var "x") (Num 100))
                   (Seq (If (Ig (Var "x") (Num 3)) Throw Skip)
                        (Atrib (Var "x") (Mul (Var "x") (Var "x")))
                   )
            )

exemploE2 :: Exp
exemploE2 = (Catch (Atrib (Var "x") (Num 0))
                   (Atrib (Var "x") (Num 1))
            )

exemploE3 :: Exp
exemploE3 = (Seq (Atrib (Var "x") (Num 0))
                 (Catch (While (Leq (Var "x") (Num 27))
                               (Seq (If (Ig (Var "x") (Num 0)) Throw Skip)
                                    (Atrib (Var "x") (Mul (Var "x") (Var "x")))
                               )
                        )
                        ((Atrib (Var "x") (Num (-1))))
                 )
            )

exemploA :: Exp
exemploA = Som (Num 3) (Som (Var "x") (Var "y"))

exemploB :: Exp
exemploB = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

exemploC :: Exp
exemploC = (While (Not (Ig (Var "x") (Num 0)))
                  (Atrib (Var "x") (Sub (Var "x") (Num 1))))

exemploErroTipo1 :: Exp
exemploErroTipo1 = (While (Var "x") (Atrib (Var "x") (Sub (Var "x") (Num 1))))
