module Repo.Snapshot
       ( Snapshot(Snapshot, unSnapshot)
       ) where

import Git (RefTarget)

import Repo.Project (Project)

newtype Snapshot r = Snapshot { unSnapshot :: [(Project, RefTarget r)] }
