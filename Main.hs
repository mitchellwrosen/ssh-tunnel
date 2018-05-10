{-# language ViewPatterns #-}

import Data.Foldable (traverse_)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process
import Text.Read (readMaybe)

data Args
  = Args Node Node
  deriving Show

data Node
  = Local String Int
  | Remote String String Int
  deriving Show

parser :: Parser Args
parser =
  Args <$> nodeParser <*> nodeParser
 where
  nodeParser :: Parser Node
  nodeParser =
    argument (maybeReader readLocalNode <|> maybeReader readRemoteNode) mempty

  readLocalNode :: String -> Maybe Node
  readLocalNode s = do
    [addr, readMaybe -> Just port] <-
      pure (split s)
    pure (Local addr port)

  readRemoteNode :: String -> Maybe Node
  readRemoteNode s = do
    [host, addr, readMaybe -> Just port] <-
      pure (split s)
    pure (Remote host addr port)

  -- Split a string on ':'
  split :: String -> [String]
  split s =
    case break (== ':') s of
      (t, []) -> [t]
      (t, _:u) -> t : split u

main :: IO ()
main = do
  args <- getArgs
  case execParserPure defaultPrefs (info parser mempty) args of
    Success (Args (Local laddr lport) (Remote host raddr rport)) -> do
      let cmd :: String
          cmd =
            "ssh -N -L " ++ laddr ++ ":" ++ show lport ++ ":" ++ raddr ++ ":"
              ++ show rport ++ " " ++ host
      putStrLn cmd
      callCommand cmd
    Success (Args (Remote host raddr rport) (Local laddr lport)) -> do
      let cmd :: String
          cmd =
            "ssh -N -R " ++ raddr ++ ":" ++ show rport ++ ":" ++ laddr ++ ":"
              ++ show lport ++ " " ++ host
      putStrLn cmd
      callCommand cmd
    _ -> do
      traverse_ (hPutStrLn stderr)
        [ "Usage: ssh-tunnel SOURCE DESTINATION"
        , ""
        , "Examples: ssh-tunnel localhost:8080 user@host:localhost:80"
        , "          ssh-tunnel user@host:localhost:80 localhost:8080"
        ]
      exitFailure
