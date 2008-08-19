module Expression
    where

{--
data Expr = C Float | Add Expr Expr | Sub Expr Expr 
	| Mul Expr Expr | Div Expr Expr
--}

data Expr = C Float | Expr :+ Expr | Expr :- Expr 
	| Expr :* Expr | Expr :/ Expr | V String | Let String Expr  Expr

evaluate::Expr->Float
eval::Expr->Float
evaluate (C x) = x

evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2



evaluate (Let x e1 e2) = evaluateWithVar x e1 e2

evaluateWithVar::String->Expr->Expr->Float
evaluateWithVar x e1 e = evaluate e
evaluateWithVar x e1 (V s)
	| x == s = evaluate e1
	| error $ "Unbound variable " ++ x
evaluateWithVar x e (e1 :+ e2) = evaluateWithVar e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2
