{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Visualize where

-- | -----------------------------------------------------------------------
-- | Visualize Service
-- | -----------------------------------------------------------------------
-- | Definition: visualizes observable results based on learner behaviour,
-- | a learner model, or a peer-learner model. A visualization service shows
-- | learning progress, engagement, collaboration patterns, etc., often using
-- | dashboards, but other means such as reports also occur.
-- |
-- | 'v' represents the output visualization format.
-- | -----------------------------------------------------------------------

-- | Transforms learner data and a model into a visual output format.
class RenderVisualization i m v | i m -> v where
  render :: i -> m -> v

-- | The generic visualize function transforms data and a model into a visual format.
visualize :: RenderVisualization i m v => i -> m -> v
visualize input model = render input model
