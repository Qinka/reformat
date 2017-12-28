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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Reformat.Parsec
  ( parsingToReformat
  ) where

import Text.Reformat.Types (Reformat(..))
import Text.Parsec (ParseError, SourceName,runParserT,Stream)

-- | Parse the string with the parser. The parser is provided with instance of @Reformat@.
parsingToReformat :: (Stream (Str t) m Char, Reformat t)
                  => SourceName -- ^ name of input source
                  -> (Str t) -- ^ string of input
                  -> m (Either ParseError t) -- ^ output
parsingToReformat = runParserT parser ()
