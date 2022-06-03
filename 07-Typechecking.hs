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
-- First, let us reintroduce our arithmetic-and-boolean expression language
-- complete with its evaluation function.

data Exp
  = ELit Int
  | ETrue
  | EFalse
  | EPlus Exp Exp
  | ELt Exp Exp
  | EIf Exp Exp Exp
  | EPair Exp Exp
  deriving Show

data Val
  = VInt Int
  | VTrue
  | VFalse
  | VPair Val Val
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

-- Recall that issues arise when we try to evaluate combinations of operations
-- that don't "make sense," for example, the expression 1 + true represented by
-- the following term:

bad :: Exp
bad = EPlus (ELit 1) ETrue

-- Is not "valid" in the sense that addition is not defined for booleans.
-- Nevertheless, our evaluation function must do something in this case. We
-- chose to raise an error as realized by our Maybe type:

badResult :: Maybe Val
badResult = evaluate bad  -- Nothing

-- We did not make much fanfare about it at the time, but this is an example of
-- checking the type of an expression at runtime. This behavior occurred
-- implicitly in the case analysis performed by our evaluate function. When
-- the arguments to EPlus evaluate to integers, i.e., VInts, we carry out the
-- addition, in all other cases, we return Nothing. In other words, we didn't
-- "just do an addition," we:
--
-- 1.  Evaluated both sides of the addition to values.
-- 2.  Checked that both sides were integers.
-- 3.  Carried out the addition if this was true; raised an error otherwise.
--
-- This second step is an example of dynamic typechecking, i.e., typechecking
-- that occurs at runtime. Before carrying out an operation, we check the
-- identity of the values to ensure they are appropriate before carrying out
-- said operation. If the types are not consistent with the operation, then
-- we raise an error, a dynamic type error, to signify this fact.
--
-- We call a language a dynamically-typed language if it primarily performs
-- dynamic typechecking. Many popular languages are dynamically-typed,
-- including:
--
-- +   Racket
-- +   Python
-- +   Javascript
--
-- Among others.
--
-- Dynamic typing is flexible and robust in that dynamic typing ensures our
-- code is safe---we never perform an inappropriate operation like add a
-- number to a boolean---and there is no overhead in terms of extra code we
-- must write to take advantage of this feature. However, dynamic typing
-- has its downsides:
--
-- +   The runtime check induces a runtime cost. While small, you can imagine
--     that the cost of such checks add up quickly if they must occur on every
--     operation. Furthermore many of these checks are unnecessary in that
--     it is "obvious" that the arguments will produce numbers.
-- +   The error is raised at the latest time possible in the overall
--     development process of our program. This seems like a good idea until
--     we note that the "root" cause of a type error might be because of an
--     error far away from the operation that finally "goes" bad as a result
--     of our bad design. For example, if we have the expression:
--
--         5 + f(x)
--
--     Suppose that f(x) returned a boolean. While the error clearly arises
--     when we try to take the boolean result from f(x) and add it, this may
--     not be the "root cause" of the problem. For example, perhaps the issue
--     is really because f expected to receive a string but x is some other
--     type. Then we would prefer to signal the error earlier, at the point
--     of the call to f, rather than at the latest time possible.
--
-- An alternative to dynamic typechecking is to check the types of our program
-- before we run the program, a process called static typechecking. Languages
-- that feature static typechecking include:
--
-- +   C
-- +   Java
-- +   Haskell
--
-- Static typechecking has complementary strengths and weaknesses compared to
-- dynamic typechecking:
--
-- +   Since static typechecking validates that a program is consistent with
--     respect to its types, runtime checks are no longer needed to ensure
--     safety, a clean win on performance.
-- +   Static typechecking allows us to catch errors "earlier" in program
--     design by enforcing type contracts between parts of our program, e.g.,
--     function call and arguments.
--
-- The major downside of static typechecking is its complexity for both
-- programmers and compiler implementors. More complex type systems demand more
-- from both parties in terms of code written and mental effort to understand
-- how the typechecker works. In the limit, the type system becomes more
-- complicated to work with than the underlying language semantics itself!
-- Despite these downsides, static typechecking is still an increasingly
-- common choice in language design. For example, many dynamic languages like
-- Python and Javascript now have typed variants that attempt to strike a
-- balance between the words of static and dynamic typing.
--
-- In light of our compiler pipeline, we can think of typechecking as an
-- analysis pass over our code. Because ASTs are our primary code representation
-- typechecking is usually described as a recursive procedure over the structure
-- of the program. Borrowing from programming language's roots in mathematical
-- logic, type systems are traditionally specified as a logic that has a
-- relatively straightforward translation into a recursive function.
--
-- A logical defines rules that establish the validity of statements made in
-- that logic. For example, in first-order logic, we might write the statement
--
--     ∀x. ∃y. f(x, y)
--
-- Which states that:
--
--     For any x, there exists a y such that f(x, y) holds.
--
-- The validity of this statement depends on the predicate f. However, we can
-- make statements that are "obviously true" and "obviously false," e.g.,
--
--     ∀p. p ∨ ¬p and ∀p. p ∨ ¬p 
--
-- With typechecking, the statements we are verifying is whether an expression
-- has a given type. We will write such a statement in the following form:
--
--     e : t
-- 
-- Where e is an expression and t is a type.  For example, we expect the
-- statement 1 + 1 : int to be valid. In contrast the statement 1 + true : bool
-- should not be valid.
--
-- The rules of our logic are traditionally given using inference rules which
-- state the conditions under which a particular typing statement is true. For
-- example, the simplest of our rules states that any integer has type int.
--
--
--     ------- [t-int]
--     n : int
--
-- The name of the rule (for reference purposes) is t-int. Below the line is the
-- conclusion of the rule---any integer literal has type int. Above the line are
-- the premises of the rule. These premises are additional facts that must be
-- true for us to assume that the conclusion of the rule is true. Because we
-- know every integer literal has type int without additional information, there
-- are no premises.
--
-- In contrast to t-int, the rule for typechecking addition has premises:
--
--     e1 : int
--     e2 : int
--     ------------- [t-plus]
--     e1 + e2 : int
--
-- We can conclude that a plus operation produces an integer whenever we
-- recursively typecheck e1 and e2 and find that they both have type int. Note
-- that we will not have a rule covering the case when e1 or e2 do not have type
-- int. The absence of such a rule implicitly indicates an error in our resulting
-- implementation!
--
-- Booleans are similar to ints. Boolean literals have simple typing rules and
-- the comparison operator over integers proceeds similarly.
--
--     ----------- [t-true]
--     true : bool
--
--     ------------ [t-false]
--     false : bool
--
--     e1 : int
--     e2 : int
--     -------------- [t-lt]
--     e1 < e2 : bool
--
-- However, typechecking a conditional is more interesting. Clearly the guard
-- must have boolean type. However, what constraints do we put on the branches,
-- if anything? Consider the following program:
--
--     1 + (if b then 1 else true)
--
-- Here, the result type of the conditional depends on the value that b
-- produces. In the true case, the conditional produces a number and the overall
-- expression is well-typed. In the false case, the conditional produces a
-- boolean and the expression is not well-typed (because we evaluate the
-- conditional to 1 + true).
--
-- How can we capture this conditional behavior in our types? Note that our
-- simple type system only allows us to assign the type int or bool to the
-- conditional. The only way we can definitely say which type to use is to
-- evaluate the guard expression, but this defeats the purpose of typechecking!
-- The whole point of the typechecking operation was to check types without
-- having to run the program.
--
-- Because of our simple type system, we must make a compromise. Even though
-- there are some programs where having conditionals whose branches have
-- different types are correct:
--
--     1 + (if true then 1 else false)
--
-- Our general-purpose rule can't account for these specific instances. We
-- instead restrict conditionals so that their branches must produces the
-- same type:
--
--     e1 : bool
--     e2 : t
--     e3 : t
--     ------------------------- [t-if]
--     if e1 then e2 else e3 : t
--
-- Note in the premises of t-if, we implicitly require that e2 and e3 have
-- the same type by using the same variable, t, in both of their respective
-- premises.
--
-- With our type system completely specified, we can now translate it into
-- code. Our typechecker is a function that, given an expression, produces
-- the type of that expression or an error if the expression does not
-- typecheck. Our rules are specified in terms of the structure of the
-- expression, so we proceed by pattern matching on the expression in
-- question. When premises are present, they become recursive calls to
-- the typechecker, and in the case of our conditional, we do an explicit
-- equality check to ensure the branches of the conditional produce the
-- same types.

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
typecheck (EPair e1 e2) =
  case (typecheck e1, typecheck e2) of
    (Just t2, Just t3) ->
      if t2 == t3 then
        Just t2
      else
        Nothing
    _ -> Nothing


-- With the typechcking function specified, we can now write a version of
-- evaluate, evaluateP, that is partial. That is, it fails implicitly in the
-- cases where we evaluate to values that we don't want (via inexhaustive
-- pattern matching). Consequently, the function is simpler because we no
-- longer need the Maybe type.
--
-- (Note that the warnings arise because Haskell does not want us to write
-- partial functions like this! We would normally provide all the cases in
-- good Haskell code.)

evaluateP :: Exp -> Val
evaluateP (ELit n) = VInt n
evaluateP ETrue = VTrue
evaluateP EFalse = VTrue
evaluateP  (EPlus e1 e2) =
  case (evaluateP e1, evaluateP e2) of
    (VInt n1, VInt n2) -> VInt $ n1 + n2
evaluateP (ELt e1 e2) =
  case (evaluateP e1, evaluateP e2) of
    (VInt n1, VInt n2) -> if n1 < n2 then VTrue else VFalse
evaluateP (EIf e1 e2 e3) =
  case evaluateP e1 of
    VTrue -> evaluateP e2
    VFalse -> evaluateP e3
evaluateP (EPair e1 e2) = VPair (evaluateP e1) (evaluateP e2)

-- To use evaluateP, we can use our typechecker to ensure that the program
-- is well-typed before running it. Thus, if the program typechecks, we know
-- that evaluateP will definitely produce a Val!

typecheckAndEvaluate :: Exp -> Maybe Val
typecheckAndEvaluate e =
  case typecheck e of
    Just _ -> Just $ evaluateP e  -- Safe! Typechecking guarantees this returns a Val!
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Exercise: previously, you added pairs to your language implementation:
--
--     e ::= ... | (e1, e2) | fst e | snd e
--     v ::= ... | (v1, v2)
--
-- Now try extending your typechecker to type check pairs. This will require
-- adding an additional type to your language:
--
--     t ::= ... | t1 * t2
--
-- As well as appropriate rules for typechecking each new expression that you
-- introduce.
--------------------------------------------------------------------------------