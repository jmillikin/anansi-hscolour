{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Anansi.HsColour
	( loomHTML
	, loomLaTeX
	, looms
	) where

import qualified Data.Map
import           Data.Text (Text)

import qualified Anansi

import           Anansi.HsColour.HTML (loomHTML)
import           Anansi.HsColour.LaTeX (loomLaTeX)

-- | Looms which use HsColour to produce colorized code blocks.
--
-- Use this with 'Anansi.defaultMain':
--
-- > #!/usr/bin/env runhaskell
-- > import Anansi
-- > import Anansi.HsColour
-- > import Data.Map
-- >
-- > main = defaultMain (unions [Anansi.looms, Anansi.HsColour.looms])
looms :: Data.Map.Map Text Anansi.Loom
looms = Data.Map.fromList
	[ ("anansi-hscolour.html", loomHTML)
	, ("anansi-hscolour.latex", loomLaTeX)
	]
