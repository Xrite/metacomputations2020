module Supercompilation where

import Deforestration
import Extraction
import Preprocessing

supercompile p = extractFromProcessTree $ buildProcessTree $ preprocess p
