{- This file is part of reformat

   Reformat is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 or the License,
   or (at your option) any later version.

   Reformat is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Yu.  If not, see <http://www.gnu.org/licenses/>.
-}

{-|
Module: Text.Reformat.Parsec
Description: Parsec for Reformat
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: GPL3
Maintainer: me@qinka.pro
Stability: experimental
Portablility: unknown


-}

module Text.Reformat.Parsec
  (
  ) where

import Text.Reformat.Types (Reformat(..), ParserError, SourceName)

parsingToReformat :: Reformat t s => SourceName -> s -> m (Either ParserError t)
parsingToReformat src str = runParserT parser () src str
