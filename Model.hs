{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model
  ( Model (..)
  ) where

-- | Represents a learner model or peer-learner model.
--
-- The model type determines the evidence type used to update it.
class Model model evidence | model -> evidence where
  initModel :: model
  update :: evidence -> model -> model
