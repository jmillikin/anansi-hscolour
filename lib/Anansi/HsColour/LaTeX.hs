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

module Anansi.HsColour.LaTeX (loomLaTeX) where

import           Control.Monad (forM_)
import           Control.Monad.Reader (asks)
import           Control.Monad.Writer (tell)
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Char8 (ByteString)
import           Data.Monoid (mappend, mconcat)
import qualified Data.Text
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Anansi hiding (loomLaTeX)
import qualified Language.Haskell.HsColour as HsColour
import           Language.Haskell.HsColour.Colourise (defaultColourPrefs)

loomLaTeX :: Loom
loomLaTeX = mapM_ putBlock . documentBlocks where
	putBlock b = case b of
		BlockText text -> tell (encodeUtf8 text)
		BlockFile path content -> do
			epath <- escape path
			let label = mconcat ["{\\bf\\(\\gg\\) ", epath, "}\n"]
			putContent label content
		BlockDefine name content -> do
			ename <- escape name
			let label = mconcat ["{\\bf\\(\\ll\\) ", ename, "\\(\\gg\\)}\n"]
			putContent label content
	
	putContent label cs = do
		tell "\\begin{quotation}\n"
		tell "\\noindent{}"
		tell label
		tell "\n\n"
		forM_ cs $ \c -> case c of
			ContentText _ text -> do
				expanded <- expandTabs text
				tell "\\noindent{}"
				tell (colorize (expanded `mappend` "\n"))
			ContentMacro _ indent name -> do
				tell "\\noindent{}"
				formatMacro indent name >>= tell
		tell "\n"
		tell "\\end{quotation}\n"
	
	formatMacro indent name = do
		escIndent <- escape indent
		escName <- escape name
		return (mconcat [escIndent, "$|$\\emph{", escName, "}$|$\n"])

escape ::  Text -> LoomM ByteString
escape text = do
	tabSize <- asks loomOptionTabSize
	
	return $ encodeUtf8 $ Data.Text.concatMap (\c -> case c of
		'\t' -> Data.Text.replicate (fromInteger tabSize) " "
		'\\' -> "\\textbackslash{}"
		'{' -> "\\{"
		'}' -> "\\}"
		'_' -> "\\_"
		_ -> Data.Text.singleton c) text

expandTabs :: Text -> LoomM Text
expandTabs txt = do
	tabSize <- asks loomOptionTabSize
	return $ Data.Text.concatMap (\c -> case c of
		'\t' -> Data.Text.replicate (fromInteger tabSize) " "
		_ -> Data.Text.singleton c) txt

colorize :: Text -> ByteString
colorize = ByteString.pack
         . HsColour.hscolour HsColour.LaTeX defaultColourPrefs False True "" False
         . ByteString.unpack
         . encodeUtf8
