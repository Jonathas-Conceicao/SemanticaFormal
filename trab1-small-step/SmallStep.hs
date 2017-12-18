import Estado

data AExp
  = Num Int
  | Var String
  | Som AExp AExp
  | Sub AExp AExp
  | Mul AExp AExp
  deriving(Show)

data BExp
  = TRUE
  | FALSE
  | Not BExp
  | And BExp BExp
  | Or  BExp BExp
  | Ig  AExp AExp
  deriving(Show)

data CExp
  = While BExp CExp
  | If BExp CExp CExp
  | Seq CExp CExp
  | Atrib AExp AExp
  | Skip
  deriving(Show)

interpretA :: (AExp, Estado) -> (AExp, Estado)
interpretA (a, s) = if isFinalA a then (a, s) else interpretA (aSmallStep (a, s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

aSmallStep :: (AExp, Estado) -> (AExp, Estado)
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

interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b, s) = if isFinalB b then (b, s) else interpretB (bSmallStep (b, s))

isFinalB :: BExp -> Bool
isFinalB TRUE  = True
isFinalB FALSE = True
isFinalB x = False

bSmallStep :: (BExp,Estado) -> (BExp,Estado)
bSmallStep (Not FALSE, s)          = (TRUE, s)
bSmallStep (Not TRUE,  s)          = (FALSE, s)
bSmallStep (Not b, s)              = (Not bn, s) where (bn, _) = bSmallStep (b, s)
bSmallStep (And TRUE  b2, s)       = (b2, s)
bSmallStep (And FALSE b2, s)       = (FALSE, s)
bSmallStep (And b1 b2, s)          = (And bn b2, s) where (bn, _) = bSmallStep (b1, s)
bSmallStep (Or FALSE b2, s)        = (b2, s)
bSmallStep (Or TRUE  b2, s)        = (TRUE, s)
bSmallStep (Or b1 b2, s)           = (Or  bn b2, s) where (bn, _) = bSmallStep (b1, s)
bSmallStep (Ig (Num x) (Num y), s) = (if x == y then TRUE else FALSE, s)
bSmallStep (Ig (Num x) b, s)       = (Ig (Num x) bn, s) where (bn, _) = aSmallStep (b, s)
bSmallStep (Ig b1      b2, s)      = (Ig bn b2, s) where (bn, _) = aSmallStep (b1, s)


interpretC :: (CExp, Estado) -> (CExp, Estado)
interpretC (c, s) = if isFinalC c then (c, s) else interpretC (cSmallStep (c, s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x    = False

cSmallStep :: (CExp, Estado) -> (CExp, Estado)
cSmallStep (Atrib (Var v) (Num x), s) = (Skip, ns) where ns = mudaVar s v x
cSmallStep (Atrib (Var v) a, s)       = (Atrib (Var v) an, s) where (an, _) = aSmallStep (a, s)
cSmallStep (Seq Skip c, s)            = (c, s)
cSmallStep (Seq c1  c2, s)            = (Seq cn c2, sn) where (cn, sn) = cSmallStep (c1, s)
cSmallStep (If TRUE  c1 c2, s)        = (c1, s)
cSmallStep (If FALSE c1 c2, s)        = (c2, s)
cSmallStep (If b     c1 c2, s)        = (If bn c1 c2, s) where (bn, _) = bSmallStep (b, s)
cSmallStep (While b c, s)             = (If b (Seq c (While b c)) (Skip), s)

meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])

exemplo3 :: CExp
exemplo3 = (While (Not (Ig (Var "x") (Num 0)))
                  (Atrib (Var "x") (Sub (Var "x") (Num 1))))
