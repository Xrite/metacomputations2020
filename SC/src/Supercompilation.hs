module Supercompilation where

import Deforestration
import Extraction
import Preprocessing
import Lang

supercompile :: MonadFail m => Program -> m Program
supercompile p = extractFromProcessTree $ buildProcessTree $ preprocess p
