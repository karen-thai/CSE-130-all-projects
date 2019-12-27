import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding

main :: IO ()                             
main = do 
  setLocaleEncoding utf8
  putStrLn(welcome)  
  loop 0 []

loop :: Int -> Nano.Env -> IO()
loop i env = do
  putStrFlush ( printf "\x03BB [%d] " i)
  task <- getLine
  case strCmd task of
      CQuit -> do 
               doQuit
               loop(i+1) env
      CRun str -> do 
                  doRun str
                  loop(i+1) env
      CEval str -> do 
                   doEval env str
                   loop(i+1) env
      CLoad str -> do
                   envs <- doLoad str
                   helper(envs) 
                   loop(i+1) (envs ++ env)
                     where 
                     helper ::Nano.Env -> IO()
                     helper ls = do 
                       putStrLn("definitions: " ++ init( printLs ls))
                       where 
                          printLs ::Nano.Env -> String
                          printLs [] = [] 
                          printLs ((xId, xVal):xs) = xId ++ " " ++ printLs xs
                   
      CUnknown -> do 
                  doUnknown
                  loop(i+1) env
--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

