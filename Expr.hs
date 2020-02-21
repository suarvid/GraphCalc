module Expr where
import Data.Maybe
import Data.Char
import Parsing
import Test.QuickCheck

--A--------------------------------------------------------------------
data Expr = Num Double | Var | BinOp Ops Expr Expr | Func Funcs Expr
  deriving (Eq)

data Ops = Add | Mul
  deriving (Eq, Show)

data Funcs = Sin | Cos
  deriving (Eq, Show)

--Smart constructors--
addSmart :: Expr -> Expr -> Expr
addSmart (Num 0) e       = e
addSmart e    (Num 0)    = e
addSmart (Num n) (Num m) = Num (n + m)
addSmart e1   e2         = BinOp Add e1 e2

mulSmart :: Expr -> Expr -> Expr
mulSmart (Num 1) e       = e
mulSmart e ( Num 1)      = e
mulSmart (Num 0) e       = (Num 0)
mulSmart e (Num 0)       = (Num 0)
mulSmart (Num n) (Num m) = (Num (m*n))
mulSmart e1 e2           = BinOp Mul e1 e2

funcSmart :: Expr -> Expr
funcSmart (Func f (Num n)) = Num(eval (Func f (Num n)) 0)
funcSmart e                = e


--For Dave, with Love-------------------------------------------------
x :: Expr
x = Var

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add e1 e2 = BinOp Add e1 e2
mul e1 e2 = BinOp Mul e1 e2

sin,cos :: Expr -> Expr
sin = Func Sin
cos = Func Cos
--B-------------------------------------------------------------------
instance Show Expr where 
    show = showExpr

showExpr :: Expr -> String
showExpr Var               = "x"
showExpr (Num n)           = show n
showExpr (BinOp Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
showExpr (BinOp op e1 e2)  = showExpr e1 ++ showOp op ++ showExpr e2
showExpr (Func fun expr)   = showFun fun ++ showArg expr


--Used to encapsulate arguments which need encapsulation to show priority.
showArg :: Expr -> String
showArg (BinOp Add e1 e2) = encaps (BinOp Add e1 e2)
showArg (BinOp Mul e1 e2) = encaps (BinOp Mul e1 e2)
showArg e                 = showExpr e

encaps :: Expr -> String
encaps e = "(" ++ showExpr e ++ ")"
showFactor :: Expr -> String
showFactor (BinOp Add e1 e2) = "(" ++ (showExpr (BinOp Add e1 e2)) ++ ")"
showFactor expression        = showExpr expression

showOp :: Ops -> String
showOp (Add) = "+"
showOp (Mul) = "*"

showFun :: Funcs -> String
showFun (Sin) = "sin "
showFun (Cos) = "cos "
--C-----------------------------------------------------------------------

eval :: Expr -> Double -> Double
eval (Num numVal) _ = numVal
eval (Var) varVal   = varVal
eval (BinOp op e1 e2) varVal = (eval e1 varVal) `op'` (eval e2 varVal)
  where op' = evalBinOp op
eval (Func func expr) varVal = evalFun func expr varVal

evalBinOp :: Ops -> (Double -> Double -> Double)
evalBinOp Add = (+)
evalBinOp Mul = (*)

evalFun :: Funcs -> Expr -> Double -> Double
evalFun Sin expr varVal = Prelude.sin (eval expr varVal)
evalFun Cos expr varVal = Prelude.cos (eval expr varVal)
--D---------------------------------------------------------------------------
-- given a string, tries to interpret it as an expression
-- if successful, returns Just expression, otherwise Nothing
-- Requirement that we use an unmodified version of the module Parsing.hs to construct this
readExpr :: String -> Maybe Expr
readExpr input = case parse expr input' of 
             Just (e,"") -> Just e
             _           -> Nothing
  where input' = filter (not . isSpace) input

numb :: Parser Expr
numb = Num <$> readsP

expr :: Parser Expr
expr = foldl1 (BinOp Add) <$> chain term (char '+') 

term :: Parser Expr
term = foldl1 (BinOp Mul) <$> chain factor (char '*') 

factor :: Parser Expr 
factor =  numb <|> (char 'x' *> return Var) <|> char '(' *> expr <* char ')' <|> cosine <|> sine

cosine :: Parser Expr
cosine = ((Func Cos) <$> k)
  where k = (char 'c') *> (char 'o') *> (char 's') *> factor
 
sine :: Parser Expr
sine = ((Func Sin) <$> k)
  where k = char 's' *> char 'i' *> char 'n' *> factor

assoc :: Expr -> Expr
assoc (BinOp Mul e1 (BinOp Mul e2 e3)) = assoc (BinOp Mul (BinOp Mul (assoc e1) (assoc e2)) (assoc e3))
assoc (BinOp Mul e1 e2)          = BinOp Mul (assoc e1) (assoc e2)
assoc (BinOp Add (BinOp Add e1 e2) e3) = assoc (BinOp Add (assoc e1) (BinOp Add (assoc e2) (assoc e3)))
assoc (BinOp Add e1          e2) = BinOp Add (assoc e1) (assoc e2)
assoc (Func fun expr)            = (Func fun (assoc expr))
assoc (Num n)                    = Num n
assoc Var                        = Var
-- E --------------------------------------
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = (assoc e) == assoc (fromJust (readExpr $ showExpr e))
 
arbExpr' :: Int -> Gen Expr
arbExpr' n = frequency [(2,Num <$> rNum),(1,return Var),(n, randomOp),(n,randomFun )]
  where
    n' = n `div` 2
    
    randomOp = do 
      a <- arbExpr' (n')
      b <- arbExpr' (n')
      op <- rOps
      return $ BinOp op (a) (b)
    randomFun = do
        a <- arbExpr' (n')
        fun <- rFunc
        return $ Func fun a
    

rOps :: Gen Ops
rOps = oneof [return Add, return Mul]

rFunc :: Gen Funcs
rFunc = oneof [return Cos,return Sin]

rNum :: Gen Double
rNum = choose (0,99)

instance Arbitrary Expr where
  arbitrary =  sized arbExpr'
  
-- F ---------------------------------------- 
simplify :: Expr -> Expr
simplify (BinOp Add e1 e2) = addSmart (simplify e1) (simplify e2)
simplify (BinOp Mul e1 e2) = mulSmart (simplify e1) (simplify e2)
simplify (Func fun expr)   = funcSmart(Func fun (simplify expr))
simplify e                 = e



--Our own prop for checking if the expression is simplified to the base cases.-----------
simplifiedEnough:: Expr -> Bool
simplifiedEnough (Var) = True
simplifiedEnough (Num n) = True
simplifiedEnough (BinOp Add (Num n1) (Num n2)) = False
simplifiedEnough (BinOp Add e1 e2) = simplifiedEnough e1 && simplifiedEnough e2
simplifiedEnough (BinOp Mul (Num n1) (Num n2)) = False
simplifiedEnough (BinOp Mul e1 e2) = simplifiedEnough e1 && simplifiedEnough e2
simplifiedEnough (Func f (Num n)) = False
simplifiedEnough (Func f e)       = simplifiedEnough e 

prop_simplifiedEnough e =
  simplifiedEnough $ simplify e


prop_SimplifyCorrect e varVal =
  abs (eval e varVal - eval (simplify e) varVal) < 0.000001  -- to avoid rounding errors
-- G ---------------------
differentiate :: Expr -> Expr
differentiate = simplify . differentiate' . simplify 
  where 
    differentiate' (Num _) = Num 0
    differentiate'  Var    = Num 1
    differentiate' (BinOp Add e1 e2) = (BinOp Add (differentiate e1) (differentiate e2))
    differentiate' (BinOp Mul e1 e2) = BinOp Add (BinOp Mul (differentiate e1) (e2)) (BinOp Mul (e1) (differentiate e2))
    differentiate' (Func Sin expr) =  (BinOp Mul (differentiate expr) (Func Cos expr))
    differentiate' (Func Cos expr) =  (BinOp Mul (differentiate expr) (BinOp Mul (Num (-1)) (Func Sin expr))) 




