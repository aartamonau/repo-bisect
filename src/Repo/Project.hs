module Repo.Project
       ( Project(Project, projectName, projectPath)
       ) where

import Data.Text (Text)

data Project = Project { projectName :: Text
                       , projectPath :: FilePath
                       }
             deriving Show
