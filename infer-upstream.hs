{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import System.Exit               (exitFailure, exitSuccess)
import Control.Applicative       ((<$>), (<*>),optional,pure)
import Data.Monoid               (mempty, (<>))
import Data.Maybe                (fromMaybe)
import Github.Data               (Repo(..), GithubOwner(..), RepoRef(..))
import Github.Repos              (userRepo)

import System.IO (hGetContents)
import System.Process (createProcess,StdStream(CreatePipe),proc,std_out)
import qualified Text.ParserCombinators.Parsec as P
import Data.Char (isSpace)
import Data.Either (isRight)
import System.Environment (getArgs, getProgName)

import qualified Options.Applicative.Builder.Internal as X
import qualified Options.Applicative                  as O
import qualified Options.Applicative.Help.Core        as C
import qualified Options.Applicative.Help.Chunk       as Ch

import qualified Text.PrettyPrint.ANSI.Leijen as L

data Repository = Repository { 
                     repositoryName :: String
                   , userName :: String
                  } deriving Show

data Options = Options { repo :: Maybe Repository
                       , usingCwd :: Bool
                       } deriving Show

main :: IO ()
main = O.execParser (O.info (O.helper <*> choices) mempty) >>= start

choices :: O.Parser Options
choices = Options
  <$> optional repository
  <*> O.switch ( O.long "using-cwd" <> 
          O.help "Use git and current directory to obtain upstream." )

repository :: O.Parser Repository
repository =  Repository
  <$> O.strOption ( O.short 'r' <> O.long "repo" <> 
          O.help "Name of the repository that we will look up." )
  <*> O.strOption ( O.short 'u' <> O.long "user" <> 
          O.help "Name of the user that we will look this repo up on." )

defStr :: String -> X.Mod X.ArgumentFields String -> O.Parser String
defStr a = def a . O.argument O.str

def :: a -> O.Parser a -> O.Parser a
def a = fmap (fromMaybe a) . O.optional

defaultPrefs = O.prefs mempty

usageHelp programName = 
  let i = (O.info (O.helper <*> choices) mempty)
      str = (`L.displayS` "")
        $ L.renderPretty 1.0 (O.prefColumns defaultPrefs)
        $ C.helpText
        $ C.usageHelp $ Ch.vcatChunks
         [  pure . C.parserUsage defaultPrefs (O.infoParser i) . unwords $ programName : []
         , fmap (L.indent 2) . O.infoProgDesc $ i ]
   in putStrLn str

start :: Options -> IO ()
start opts = do
  r <-
    case repo opts of
      Nothing -> case usingCwd opts of 
        True -> obtainOriginFromCurrentDirectory >>= \res -> 
          case res of 
            (Just (repo,user)) -> return (Just (Repository repo user))
            _ -> putStrLn "Could not find git origin remote for current directory." >> return Nothing
        False -> getProgName >>= usageHelp >> return Nothing
      _ -> return (repo opts)
  case r of 
    Just (Repository repo user) -> do
      d <- userRepo user repo
      case d of
       Right repo -> inferUpstream repo
       Left  err   -> do 
        putStrLn $ show err
        exitFailure
    _ -> exitFailure

obtainUpstreamRepo :: RepoRef -> IO ()
obtainUpstreamRepo (RepoRef ghUser repoName) = do
    rd <- userRepo (githubOwnerLogin ghUser) repoName
    case rd of
      -- Print put the ssh repo url
      Right repo -> putStrLn (repoSshUrl repo)
      Left  err -> do 
        putStrLn $ show err
        exitFailure

inferUpstream :: Repo -> IO ()
inferUpstream repo = do
  case (repoParent repo) of
      Just repoParent     -> obtainUpstreamRepo repoParent
      Nothing             -> exitSuccess -- No upstream, say nothing.

originParser = do 
  sequence (map P.char "origin")
  P.many1 P.space
  sequence (map P.char "https://github.com/")
  user <- P.many1 (P.satisfy ( \x -> not (isSpace x || x=='/') ))
  P.char '/'
  repo <- P.many1 (P.satisfy ( \x -> not (isSpace x || x=='/') ))
  return (repo,user)

obtainOriginFromCurrentDirectory = do
  (_, Just hout, _, _) <-
       createProcess (proc "git" ["remote","-v"]){ std_out = CreatePipe }
  originInfo <- hGetContents hout >>= return . lines >>= return . take 1 . filter isRight . map (P.parse originParser "")
  return (case originInfo of 
   [Right x] -> Just x
   _ -> Nothing)
