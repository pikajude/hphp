module Language.HPHP.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Functor
import qualified Data.Text as T
import Debug.Trace
import Language.HPHP.AST hiding (concat)

expression = try var <|> stringLiteral <|> concatenation <|> groupedExpr
    where groupedExpr = char '(' *> expression <* char ')'

stringLiteral = trace "string" $ singleQuoted <|> doubleQuoted
    where singleQuoted = StringLiteral <$> (char '\'' *> escapedWith '\'' <* char '\'')
          doubleQuoted = StringLiteral <$> (char '"' *> escapedWith '"' <* char '"')
          escapedWith c = T.concat <$> many (string "\\\\" <|> string (T.pack ['\\', c]) <|> (T.singleton <$> notChar c))

var = trace "var" $ simpleVar <|> simpleVarVar <|> complexVar
    where simpleVar = do char '$'
                         start <- satisfy $ inClass "a-zA-Z_"
                         end <- takeWhile $ inClass "a-zA-Z0-9_"
                         return . Var . StringLiteral $ T.cons start end
          simpleVarVar = Var <$> (char '$' *> var)
          complexVar = do string "${"
                          m <- expression
                          char '}'
                          return $ Var m

concatenation = trace "concat" $ do e1 <- expression
                                    many space
                                    char '.'
                                    many space
                                    e2 <- expression
                                    return $ Concat e1 e2
