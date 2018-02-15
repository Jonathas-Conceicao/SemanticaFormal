import Tipo
import Estado
import SmallStep

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
exemploErroTipo1 = (Atrib (Var "x") TRUE)

exemploErroTipo2 :: Exp
exemploErroTipo2 = (While (Var "x") (Atrib (Var "x") (Sub (Var "x") (Num 1))))

exemploErroTipo3 :: Exp
exemploErroTipo3 = (While (Not (Ig (Var "x") (Num 0)))
                     (Sub (Var "x") (Som TRUE TRUE))
                   )
