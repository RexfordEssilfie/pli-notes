module Typechecking where

-- TODO: need to finish!

--------------------------------------------------------------------------------
-- Typechecking
--------------------------------------------------------------------------------

-- In addition to pretty printing and interpreting code which effectively
-- translates the code from one representation to another, we can also perform
-- analyses on the code, either for the purposes of verification or
-- optimization. The most prevalent of these analyses is typechecking which
-- allows us determine whether a program is consistent with respect to its types
-- without actually running the program.
--
-- First, let us reintroduce our arithmetic-and-boolean expression language:

data Exp
  = ELit Int
  | ETrue
  | EFalse
  | EPlus Exp Exp
  | ELt Exp Exp
  | EIf Exp Exp Exp
  deriving Show

data Val
  = VInt Int
  | VTrue
  | VFalse
  deriving Show

data Typ
  = TInt
  | TBool
  deriving (Show, Eq)

evaluate :: Exp -> Maybe Val
evaluate (ELit n) = Just (VInt n)
evaluate ETrue = Just VTrue
evaluate EFalse = Just VTrue
evaluate (EPlus e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just (VInt n1), Just (VInt n2)) -> Just $ VInt $ n1 + n2
    _                                -> Nothing
evaluate (ELt e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just (VInt n1), Just (VInt n2)) -> Just $ if n1 < n2 then VTrue else VFalse
    _                                -> Nothing
evaluate (EIf e1 e2 e3) =
  case evaluate e1 of
    Just VTrue -> evaluate e2
    Just VFalse -> evaluate e3
    _ -> Nothing

typecheck :: Exp -> Maybe Typ
typecheck (ELit n) = Just TInt
typecheck ETrue = Just TBool
typecheck EFalse = Just TBool
typecheck (EPlus e1 e2) =
  case (typecheck e1, typecheck e2) of
    (Just TInt, Just TInt) -> Just TInt
    _ -> Nothing
typecheck (ELt e1 e2) =
  case (typecheck e1, typecheck e2) of
    (Just TInt, Just TInt) -> Just TBool
    _ -> Nothing
typecheck (EIf e1 e2 e3) =
  case (typecheck e1, typecheck e2, typecheck e3) of
    (Just TBool, Just t2, Just t3) ->
      if t2 == t3 then
        Just t2
      else
        Nothing
    _ -> Nothing

typecheckAndEvaluate :: Exp -> Maybe Val
typecheckAndEvaluate e =
  case typecheck e of
    Just _ -> evaluate e  -- Safe! Typechecking guarantees this returns a Val!
    Nothing -> Nothing