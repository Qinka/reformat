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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Reformat.Render
  ( renderFromReformat
  ) where

import Text.Reformat.Types (Reformat(..), Value(..))
import Text.Parsec
import Text.Printf


data Render = Text String -- ^ Plain text
            | Var  String String -- ^ variable with name and format
            | Nil
            deriving (Show,Eq)

-- | parser of variable
parserVar :: Stream String m Char => ParsecT String () m [Render]
parserVar = do
  char '$' >> char '{' >>  spaces
  varName <- many1 (lower <|> digit <|> char '_' <|> char '-')
  spaces >> char ':' >>  spaces
  format <-  many1 $ oneOf "cdoxXbufFgGeEs+-0123456789."
  spaces >>  char '}'
  (Var varName ('%':format):) <$> parserRender

-- | parser of text
parserText :: Stream String m Char => ParsecT String () m [Render]
parserText = do
  text  <- many1 $ noneOf "$"
  (Text text:) <$> parserRender

-- | parser of dollar
parserD :: Stream String m Char => ParsecT String () m [Render]
parserD = string "$$" >> (Text "$":) <$> parserRender

-- | parser of string's EOF
parserEOF :: Stream String m Char => ParsecT String () m [Render]
parserEOF = eof >> return [Nil]

-- | parser of render(s)
parserRender :: Stream String m Char => ParsecT String () m [Render]
parserRender = try parserEOF <|> try parserText <|> try parserD <|> parserVar

-- | render the string with given format and item
renderFromReformat :: (Monad m, Reformat t, Str t ~ String)
                   => t   -- ^ item
                   -> String -- ^ format string 
                   -> m (Either ParseError String) -- output
renderFromReformat t str = (concat . step <$>) <$> runParserT parserRender () "render format" str
  where pair = renderPair t
        step [] = []
        step (Text s:rs) = s:step rs
        step (Var v f:rs) = prt f (pair v):step rs
        step (Nil:rs) = step rs
        prt f (I i) = printf f i
        prt f (R r) = printf f r
        prt f (S s) = printf f s
        prt _ N     = ""


