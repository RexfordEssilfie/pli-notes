module FunctionsAndBinding where

import qualified Data.Map as M

-- TODO: write me!

data Exp
  = ELit Int
  | EBool Bool
  | EPlus Exp Exp
  | ELt Exp Exp
  | EIf Exp Exp Exp
  | EVar String
  | ELam String Exp
  | EApp Exp Exp
  deriving (Show, Eq)

data Val
  = VInt Int
  | VBool Bool
  | VLam String Exp
  deriving (Show, Eq)

parens :: String -> String
parens s = "(" ++ s ++ ")"

spaces :: [String] -> String
spaces = unwords

printExp :: Exp -> String
printExp (ELit n) = show n
printExp (EBool True) = "true"
printExp (EBool False) = "false"
printExp (EPlus e1 e2) = parens . spaces $ [printExp e1, "+", printExp e2]
printExp (ELt e1 e2) = parens . spaces $ [printExp e1, "<", printExp e2]
printExp (EIf e1 e2 e3) = parens . spaces $ ["if", printExp e1, "then", printExp e2, "else", printExp e3]
printExp (EVar x) = x
printExp (ELam x e) = parens . spaces $ ["\\" ++ x, "->", printExp e]
printExp (EApp e1 e2) = parens . spaces $ [printExp e1, printExp e2]

substitute :: Exp -> String -> Exp -> Exp
substitute v x = go
  where
    go e@(ELit _) = e
    go e@(EBool _) = e
    go (EPlus e1 e2) = EPlus (go e1) (go e2)
    go (ELt e1 e2) = ELt (go e1) (go e2)
    go (EIf e1 e2 e3) = EIf (go e1) (go e2) (go e3)
    go e@(EVar y)
      | x == y = v
      | otherwise = e
    go e@(ELam y e1)
      | x == y = e
      | otherwise = ELam y (go e1)
    go (EApp e1 e2) = EApp (go e1) (go e2)

valToExp :: Val -> Exp
valToExp (VInt n) = ELit n
valToExp (VBool b) = EBool b
valToExp (VLam x e) = ELam x e

evaluate :: Exp -> Maybe Val
evaluate (ELit n) = Just $ VInt n
evaluate (EBool b) = Just $ VBool b
evaluate (EPlus e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just (VInt n1), Just (VInt n2)) -> Just $ VInt $ n1 + n2
    _ -> Nothing
evaluate (ELt e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just (VInt n1), Just (VInt n2)) -> Just $ VBool $ n1 < n2
    _ -> Nothing
evaluate (EIf e1 e2 e3) =
  case evaluate e1 of
    Just (VBool True) -> evaluate e2
    Just (VBool False) -> evaluate e3
    _ -> Nothing
evaluate e@(EVar _) = Nothing
evaluate (ELam x e) = Just $ VLam x e
evaluate (EApp e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just (VLam x e), Just v) -> evaluate $ substitute (valToExp v) x e
    _ -> Nothing

type Binding = (String, Exp)
type Program = ([Binding], Exp)

evaluateProg :: Program -> Maybe Val
evaluateProg (binds, e) = evaluate e'
  where
    e' = foldl (\e1 (x, v) -> substitute v x e1) e binds