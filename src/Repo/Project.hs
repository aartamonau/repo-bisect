{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Repo.Project
       ( Project(Project, projectName, projectPath)
       , getProjectHead
       , findCommit
       -- TODO: should not be exported
       , withProject
       ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)

import Data.Maybe (fromMaybe)
import Data.Tagged (Tagged(Tagged))
import Data.Text (Text)

import Git (RefTarget,
            Commit(commitParents),
            lookupReference, lookupCommit)

import Repo.Git (Git, WithFactory, withGitRepo, resolveRef)
import Repo.Monad (Repo)

data Project = Project { projectName :: Text
                       , projectPath :: FilePath
                       }
             deriving Show

withProject :: WithFactory n r => Project -> Git n a -> Repo a
withProject = withGitRepo . projectPath

getProjectHead :: WithFactory n r => Project -> Repo (RefTarget r)
getProjectHead proj =
  withProject proj $ do
    ref <- lift $ lookupReference "HEAD"
    return $ fromMaybe (error $ "could not resolve HEAD at " ++ projectPath proj) ref

findCommit :: WithFactory n r
           => Project -> RefTarget r -> (Commit r -> Bool) -> Repo (Maybe (Commit r))
findCommit proj head p =
  withProject proj $ do
    headCommitOid <- Tagged <$> resolveRef head
    go headCommitOid

  where go cid = do
          commit <- lift $ lookupCommit cid
          if p commit
            then return $ Just commit
            else
              case commitParents commit of
                [] -> return Nothing
                (first : _) -> go first
