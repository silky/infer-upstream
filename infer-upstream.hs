{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Prelude
import System.Exit               (exitFailure, exitSuccess)
import Control.Applicative       ((<$>), (<*>))
import Data.Monoid               (mempty, (<>))
import Data.Maybe                (fromMaybe)
import Github.Data               (Repo(..), GithubOwner(..), RepoRef(..))
import Github.Repos              (userRepo)

import qualified Options.Applicative.Builder.Internal as X
import qualified Options.Applicative                  as O

data Options = Options { repo :: String
                       , user :: String
                       } deriving Show

main :: IO ()
main = O.execParser (O.info (O.helper <*> options) mempty) >>= start


options :: O.Parser Options
options = Options
  <$> O.strOption ( O.short 'r' <> O.long "repo" <> 
          O.help "Name of the repository that we will look up." )
  <*> O.strOption ( O.short 'u' <> O.long "user" <> 
          O.help "Name of the user that we will look this repo up on." )


defStr :: String -> X.Mod X.ArgumentFields String -> O.Parser String
defStr a = def a . O.argument O.str


def :: a -> O.Parser a -> O.Parser a
def a = fmap (fromMaybe a) . O.optional


start :: Options -> IO ()
start opts = do
  d <- userRepo (user opts) (repo opts)
  case d of
    Right repo  -> inferUpstream repo opts
    Left  err   -> do 
        putStrLn $ show err
        exitFailure


obtainUpstreamRepo :: RepoRef -> IO ()
obtainUpstreamRepo (RepoRef ghUser repoName) = do
    rd <- userRepo (githubOwnerLogin ghUser) repoName
    case rd of
      -- Print put the ssh repo url
      Right repo -> putStrLn (repoSshUrl repo)
      Left  err   -> do 
        putStrLn $ show err
        exitFailure


inferUpstream :: Repo -> Options -> IO ()
inferUpstream repo opts = do
  case (repoParent repo) of
      Just repoParent     -> obtainUpstreamRepo repoParent
      Nothing             -> exitSuccess -- No upstream, say nothing.
