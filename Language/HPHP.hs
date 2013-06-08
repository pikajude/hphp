module Language.HPHP where

import Data.Attoparsec.Text
import Language.HPHP.Parser

test = print $ parseOnly (expression >> endOfInput) "$a.$b"

{-
var
Left "endOfInput"
 -}
