module Eval(eval,
            evalInEnv)
       where

import qualified Global as G
import qualified Forms as F
import qualified Environment as E

data EvalError = EvalError {badForm :: F.Form, msg :: String, more :: Maybe EvalError} deriving Show

eval :: F.Form -> Either EvalError F.Form
eval = evalInEnv G.globalEnv

evalInEnv :: E.Env -> F.Form -> Either EvalError F.Form
evalInEnv e (F.Nil) = Right F.Nil
evalInEnv e (F.Number n) = Right $ F.Number n
evalInEnv e (F.Function f) = Right $ F.Function f
evalInEnv e (F.Symbol s) =
  let val = E.lookup s e in
  case val of
    Nothing -> Left $ EvalError { badForm = F.Symbol s,
                                  msg = "Couldn't find Symbol binding in Environment.",
                                  more = Nothing }
    Just f -> evalInEnv e f
evalInEnv e (F.FunctionCall {F.func = func, F.args = args}) =
  let evaledFunc = evalInEnv e func
      evaledArgs = map (evalInEnv e) args in
  -- check func to see if it's an error
  case evaledFunc of
    Left err -> Left err
    Right f ->
      -- func not an error
      -- now check to make sure func is in fact a Function
      case f of
        F.Function f ->
          -- Func is a function
          -- now check args to see if they are errors.
          let firstPossibleError = checkArgs evaledArgs in
          case firstPossibleError of
            -- args are fine, evaluate the function on the args
            Nothing -> Right $ f (forceArgs evaledArgs)
            Just err -> Left $ EvalError { badForm = F.FunctionCall {F.func = func, F.args = args},
                                           msg = "Problem with argument in FunctionCall.",
                                           more = Just err }
        _ -> Left $ EvalError { badForm = func,
                                msg = "Form is not a Function.",
                                more = Nothing }

checkArgs :: [Either EvalError F.Form] -> Maybe EvalError
checkArgs ((Left err):xs) = Just err
checkArgs ((Right val):xs) = checkArgs xs
checkArgs [] = Nothing

forceArgs :: [Either EvalError F.Form] -> [F.Form]
forceArgs ((Right val):xs) = val:forceArgs xs
forceArgs [] = []

agEval :: String -> [Either EvalError F.Form]
agEval s = map eval $ F.agProcess s

emain = agEval "+"
