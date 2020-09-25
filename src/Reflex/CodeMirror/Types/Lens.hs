{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.CodeMirror.Types.Lens where

import Control.Lens
import Reflex.CodeMirror.Types.Types

makeClassy ''Configuration
makeClassy ''LineChar
