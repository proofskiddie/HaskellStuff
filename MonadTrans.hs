
module MonadTrans where
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map.Strict as Map

type Name = String
data Exp = Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp --lambda abstraction
  | App Exp Exp
  deriving (Show)
data Value = IntVal Integer
  | FunVal Env Name Exp
  deriving (Show)
type Env = Map.Map Name Value -- mapping from names to values

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = env Map.! n
eval0 env (Plus e1 e2) = case (eval0 env e1, eval0 env e2) of
                         (IntVal i, IntVal j) -> IntVal (i + j)
eval0 env (Abs n e)   = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                             FunVal env' n body -> eval0 (Map.insert n val2 env') body
                               

exExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- eval0 Map.empty exExp == IntVal 18



-- converting to a monadic style

-- once rewritten in terms of the identity monad it is not difficult to change to other
-- monads (using transformers)
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ env Map.! n
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                             FunVal env' n body -> eval1 (Map.insert n val2 env') body

-- string is the type that is thrown, could be extended to include source code location
type Eval2 a = ExceptT String Identity a
runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

myeval2 :: Monad m => Env -> Exp -> m Value
myeval2 env (Lit i) = return $ IntVal i
myeval2 env (Var n) = return $ env Map.! n
myeval2 env (Plus e1 e2) = do IntVal i1 <- myeval2 env e1
                              IntVal i2 <- myeval2 env e2
                              return $ IntVal (i1 + i2)
myeval2 env (Abs n e) = return $ FunVal env n e
myeval2 env (App e1 e2) = do val1 <- myeval2 env e1
                             val2 <- myeval2 env e2
                             case val1 of
                               FunVal env' n body ->
                                 myeval2 (Map.insert n val2 env') body


-- not quite correct, will not use error throwing capabilities of ExceptT monad
-- also Plus is not implemented to handle errors
eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = return $ env Map.! n
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                              FunVal env' n body -> eval2a (Map.insert n val2 env') body

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = return $ env Map.! n
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                             e2' <- eval2b env e2
                             case (e1', e2') of
                               (IntVal i, IntVal j) -> return $ IntVal (i + j)
                               _ -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                              FunVal env' n body -> eval2b (Map.insert n val2 env') body
                              _ -> throwError "type error"


eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                      Nothing -> throwError ("unbound variable : " ++ n)
                      Just val -> return val
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of
                               (IntVal i, IntVal j) -> return $ IntVal (i + j)
                               _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do val1 <- eval2 env e1
                           val2 <- eval2 env e2
                           case val1 of
                              FunVal env' n body -> eval2 (Map.insert n val2 env') body
                              _ -> throwError "type error in application"


-- can use the Reader monad to hide the environment

type Eval3 a = ReaderT Env (ExceptT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runExceptT(runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                     Nothing -> throwError ("unbound variable : " ++ n)
                     Just val -> return val
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                          (IntVal i, IntVal j) -> return $ IntVal (i + j)
                          _ -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         FunVal env' n body ->
                           local (const (Map.insert n val2 env'))
                             (eval3 body)   
                         _ -> throwError "type error in application"



--can use writerT monad transformer to add loggin functionality to code

