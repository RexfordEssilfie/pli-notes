module Interpretation where

--------------------------------------------------------------------------------
-- Interpretation
--------------------------------------------------------------------------------

-- Previously, we introduced the abstract syntax tree, a tree-like
-- representation of a computer program amendable to analysis and
-- transformation. ASTs are the core data structure manipulated by program
-- analysis tools such as compilers.
--
-- As a concrete example of an AST, we introduced a core language of arithmetic
-- expressions as well as basic functions---pretty printing and
-- interpretation---over them. Let's further refine that language to include
-- boolean values and operations over them:
--
-- e ::= n | e + e
--     | true | false | e < e | if e1 then e2 else e3
--
-- Booleans are named thusly because there are two possible values of boolean
-- type: true and false. Operations that generate booleans include comparisons
-- and logical operators. We use the less than operator as an exemplar for these
-- various operations. Finally, we have the conditional expression which
-- consumes a boolean to produce branching behavior. In the conditional `if e1
-- then e2 else e3`, e1 is the boolean expression (or "guard"), e2 is the
-- expression evaluated if the guard is true (the "if-branch"), and e3 is the
-- expression evaluated if the guard is false (the "else-branch").
--
-- Here is a version of our Exp datatype with these additional constructs:

data Exp
  = ELit Int        -- n
  | ETrue           -- true
  | EFalse          -- false
  | ENot Exp        -- ~e1
  | EAnd Exp Exp    -- e1 && e2
  | EOr Exp Exp     -- e1 || e2
  | EPlus Exp Exp   -- e1 + e2
  | EMult Exp Exp   -- e1 * e2
  | ELt Exp Exp     -- e1 < e2
  | EIf Exp Exp Exp -- if e1 then e2 else e3
  | EPair Exp Exp   -- (e1, e2)
  | EFst Exp        -- First of (e1, e2) -> e1
  | ESnd Exp        -- Second of (e1, e2) -> e2
  deriving (Show, Eq)

-- As well as a pretty-printing function for it:

parens :: String -> String
parens s = "(" ++ s ++ ")"

prettyPrintExp :: Exp -> String
prettyPrintExp (ELit n) = show n
prettyPrintExp ETrue = "true"
prettyPrintExp EFalse = "false"
prettyPrintExp (ENot e1) = parens . unwords $ ["not", prettyPrintExp e1]
prettyPrintExp (EPlus e1 e2) =
  parens . unwords $ [prettyPrintExp e1, "+", prettyPrintExp e2]
prettyPrintExp (EMult e1 e2) =
  parens . unwords $ [prettyPrintExp e1, "*", prettyPrintExp e2]
prettyPrintExp (ELt e1 e2) =
  parens . unwords $ [prettyPrintExp e1, "<", prettyPrintExp e2]
prettyPrintExp (EAnd e1 e2) =
  parens . unwords $ [prettyPrintExp e1, "&&", prettyPrintExp e2]
prettyPrintExp (EOr e1 e2) =
  parens . unwords $ [prettyPrintExp e1, "||", prettyPrintExp e2]
prettyPrintExp (EIf e1 e2 e3) =
  parens . unwords $ ["if", prettyPrintExp e1, "then", prettyPrintExp e2, "else", prettyPrintExp e3]
prettyPrintExp (EPair e1 e2) =
  parens . unwords $ [prettyPrintExp e1, ",", prettyPrintExp e2]
prettyPrintExp (EFst e1) =
  parens . unwords $ ["fst", prettyPrintExp e1]
prettyPrintExp (ESnd e1) =
  parens . unwords $ ["snd", prettyPrintExp e1]

-- What about evaluation? Recall the type signature of our evaluation function.
--
-- evaluate :: Exp -> Integer
--
-- How do we deal with expressions that produce boolean results? One approach is
-- to represent a boolean value as an integer, e.g., assigning false the value 0
-- and true any non-zero value ala C. While a valid choice, this has all the
-- problems of C booleans: now we can intermix boolean and integer expressions
-- in unintuitivee ways, e.g., 1 + (2 > 3) does not really "make sense."
--
-- It is better design to distinguish between two different kinds of values:
-- integral values and boolean values. We can do this with an additional
-- datatype:

data Val
  = VInt Int        -- n
  | VString [Char]  -- s
  | VTrue           -- true
  | VFalse          -- false
  | VPair Val Val   -- (x, y)
  deriving (Show, Eq)

-- And now our evaluate function can produce this as output. Let's try pushing
-- the implementation through:

evaluate' :: Exp -> Val
evaluate' (ELit n) = VInt n
evaluate' ETrue = VTrue
evaluate' EFalse = VFalse
evaluate' (EPlus e1 e2) =
  -- `evaluate' e1 + evaluate' e2` produces a compiler error!
  undefined
evaluate' _ =
  -- Catch-all case to cover the remaining, unimplemented cases 
  undefined

-- If we try to use `e1 + e2` as the implementation for `EPlus`, we receive
-- the following compiler error:
--
-- Interp.hs:83:3: error:
--     • No instance for (Num Val) arising from a use of ‘+’
--     • In the expression: evaluate' e1 + evaluate' e2
--       In an equation for ‘evaluate'’:
--           evaluate' (EPlus e1 e2) = evaluate' e1 + evaluate' e2
--    |
-- 84 |   evaluate' e1 + evaluate' e2
--    |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--
-- What's the problem? Recall that we changed evaluate' to produce a Val in
-- order to support boolean values. However, now that choice is coming back to
-- haunt us: we simply can't add the results of the recursive calls any more. We
-- need to pattern match on the results using a case expression and when we
-- receive integers, we can add them.

evaluate'' :: Exp -> Val
evaluate'' (EPlus e1 e2) =
  case evaluate'' e1 of
    VInt n1 ->
      case evaluate'' e2 of
        VInt n2 -> VInt $ n1 + n2
        _ -> undefined  -- Uh, what to do in this case?
    _ -> undefined  -- ... and, uh what to do here, too?
evaluate'' _ =
  -- Catch-all case to cover the remaining, unimplemented cases 
  undefined

-- Note that we can be slightly more concise here by pairing up the results
-- of the recursive calls and then immediately performing case analysis on
-- them:
--
-- case (evaluate'' e1, evaluate'' e2) of
--   (VInt n1, VInt n2) -> VInt $ n1 + n2
--   _ -> undefined -- Uh, still don't know what to do here...
--
-- No matter the approach, we run into yet another problem: it isn't clear
-- what to do in the other branches of the case expression!
--
-- Taking a step back, we can see that these cases arise when either the
-- left-hand or right-hand side of the addition operator produce a non-integer
-- value. What should we do in these cases? Issue some kind of error during
-- interpretation, like Racket! The problem is that our return type Val does not
-- allow us to produce a "non-Val" answer.
--
-- The solution to this problem is even more algebraic datatypes! Here, we we
-- need a type that allows us to distinguish between having a value and not
-- having a value, the latter case arising due to an error of some kind. This
-- is the purpose of the Maybe datatype found in the standard library:
--
-- data Maybe a
--   = Just a
--   | Nothing
--
-- We can think of a `Maybe a` value as a box around a potential value of type
-- `a`. The `Just` constructor represents the situation where the box has a
-- value. In this case, the argument to `Just` of type `a` is precisely the
-- value contained in the box. The `Nothing` constructor represents an empty
-- box, usually due to an error of some kind in our code.
--
-- With the `Maybe` datatype, we can refine our evaluate function again to
-- now account for the possibility of error in our evaluation.

vBool :: Val -> Val
vBool VFalse = VFalse
vBool VTrue = VTrue
vBool (VInt n) = if n == 0 then VFalse else VTrue
vBool (VString s) = if null s then VFalse else VTrue
vBool (VPair x y) = VTrue


evaluate :: Exp -> Maybe Val
evaluate (ELit n) = Just (VInt n)
evaluate ETrue = Just VTrue
evaluate EFalse = Just VFalse
evaluate (EPlus e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just (VInt n1), Just (VInt n2)) -> Just $ VInt $ n1 + n2
    _                                -> Nothing
evaluate (EMult e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just (VInt n1), Just (VInt n2)) -> Just $ VInt $ n1 * n2
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
evaluate (ENot e1) =
  case evaluate e1 of
    Just VTrue -> Just VFalse
    Just VFalse -> Just VTrue
    Just v      -> Just $ vBool v
    _ -> Nothing
evaluate (EAnd e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just v1, Just v2) ->
      case (vBool v1, vBool v2) of
        (VTrue, VTrue) -> Just VTrue
        _              -> Just VFalse
    _ -> Nothing
evaluate (EOr e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just v1, Just v2) ->
      case (vBool v1, vBool v2) of
        (VFalse, VFalse) -> Just VFalse
        _                -> Just VTrue
    _ -> Nothing
evaluate (EPair e1 e2) =
  case (evaluate e1, evaluate e2) of
    (Just v1, Just v2) -> Just (VPair v1 v2)
    _ -> Nothing
evaluate (EFst e1) =
  case evaluate e1 of
    Just (VPair v1 v2) -> Just v1
    _ -> Nothing
evaluate (ESnd e1) =
  case evaluate e1 of
    Just (VPair v1 v2) -> Just v2
    _ -> Nothing
-- Now our function is complete! Note that the Maybe datatype introduces a
-- second box to our results that we must wrap and unwrap throughout the
-- function. For example, in the ELit case, we must wrap the result integer
-- `n` in a Val box (VInt) which is then wrapped in a Maybe box (Just).
-- In the EPlus case, we have to unwrap the results of the recursive calls,
-- Maybe Vals, ensure that they are indeed integers, perform the desired
-- computation, and then re-wrap the results. In cases where we experience
-- a runtime error due to a type mismatch, we return Nothing.

--------------------------------------------------------------------------------
-- Exercise: try adding additional boolean operations such as negation, logical
-- conjunction, or other comparison operators to the language. For an extra
-- challenge, considering a new type, pairs. Pairs add the following new
-- syntax to our language:
--
-- e ::= (e1, e2) | fst e | snd e
--
-- The first form, the pair literal, creates a pair from two subexpressions. The
-- second and third forms are the pair projection operators, taking a pair as
-- input and returning the first and second components of the pair,
-- respectively.
--------------------------------------------------------------------------------

e1 = EMult (EFst (EPair (ELit 1) (ELit 2))) (ELit (-4))