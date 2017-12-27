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
Module: Text.Reformat.Render
Description: Render of Reformat
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: GPL3
Maintainer: me@qinka.pro
Stability: experimental
Portablility: unknown


-}

module Text.Reformat.Render
  (
  ) where

import Text.Reformat.Types (Reformat(..), Value(..))
import Text.Parsec


data Render = Text String -- ^ Plain text
            | Var  String String -- ^ variable with name and format

parserVar :: Stream String m => ParsecT String () m Render
parserVar = do
  char '$'
  char '{'
  spaces
  varName <- many1 (lower <|> digit <|> char '_' <|> char '-')
  spaces
  char ':'
  spaces
  format  <- many1 $ oneOf "cdoxXbufFgGeEs+-0123456789."
  spaces
  char '}'
  return $ Var varName spaces

parserText :: 
