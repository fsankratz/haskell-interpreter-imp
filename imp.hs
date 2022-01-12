import Data.Function
import Data.List

type Var = String

infixl 5 :=:, :>:, :||:, :&&:
infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Exp 
  = C Int          -- constant int
  | V Var          -- variable
  | Exp :+: Exp    -- addition
  | Exp :-: Exp    -- subtraction
  | Exp :*: Exp    -- multiplication
  | Exp :/: Exp    -- division
  | Exp :=: Exp    -- equality
  | Exp :>: Exp    -- ordering
  | Exp :||: Exp   -- logical or
  | Exp :&&: Exp   -- logical and
  | Not Exp        -- negation

infix 1 :=

data Stmt
  = Var := Exp     -- assignment
  | While Exp Stmt -- loop
  | Seq [Stmt]     -- sequence
  | If Exp Stmt Stmt -- conditional

type Prog = Stmt

type Val = Int
type Store = [(Var, Val)]

eval :: Exp -> Store -> Val
eval (C n) r        = n
eval (V x) r        = case lookup x r of
                        Nothing -> error ("unbound variable `" ++ x ++ "'")
                        Just v -> v
eval (e1 :+: e2) r  = eval e1 r + eval e2 r 
eval (e1 :-: e2) r  = eval e1 r - eval e2 r 
eval (e1 :*: e2) r  = eval e1 r * eval e2 r 
eval (e1 :/: e2) r  = eval e1 r `div` eval e2 r 
eval (e1 :=: e2) r  = if eval e1 r == eval e2 r then 1 else 0 
eval (e1 :>: e2) r  = if eval e1 r > eval e2 r then 1 else 0 
eval (e1 :&&: e2) r  = if eval e1 r == 1 && eval e2 r == 1 then 1 else 0 
eval (e1 :||: e2) r  = if eval e1 r == 1 || eval e2 r == 1 then 1 else 0 
eval (Not e) r  = if eval e r == 1 then 0 else 1 

exec :: Stmt -> Store -> Store
exec (x := e) r     = (x, eval e r) : r
exec (While e s) r  = exec (If e (Seq [s, (While e s)]) s) r
exec (Seq []) r     = r
exec (Seq (s:ss)) r = exec (Seq ss) (exec s r)
exec (If e1 s1 s2) r = if eval e1 r == 1 then exec s1 r else exec s2 r

run :: Prog -> Store -> Store
run p r = nubBy ((==) `on` fst) $ exec p r

fib :: Prog
fib = Seq
  [ "x" := C 0
  , "y" := C 1
  , While (V "n") $ Seq
    [ "z" := V "x" :+: V "y"
    , "x" := V "y"
    , "y" := V "z"
    , "n" := V "n" :-: C 1
    ]
  ]

fac' :: Prog
fac' = Seq
  [ "x" := C 1
  , "i" := C 1
  , (While (V "n" :>: V "i") (Seq
    [ "x" := V "x" :*: V "i"
    , "i" := V "i" :+: C 1
    ]))
  ]
