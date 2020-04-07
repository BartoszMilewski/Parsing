module BlockParser where
  
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding (many, (<|>))

data Item =
    Text String
  | Block String [Item]
  deriving Show

type CharParser st = Parsec [Char] st

-- A item is either a block delimited
-- by \begin{name} \end{name}
-- or plain text
item :: CharParser st Item
item =  
  do
    blockName <- begin
    Block blockName <$> blockBody <* end blockName
  <|> text

-- series of items (including nested blocks)
blockBody :: CharParser st [Item]
blockBody = many item

-- \begin{name}
-- returns name or fails
begin :: CharParser st String
begin = try $ string "\\begin" *> braced

-- tries \end{str} for a given string
end :: String -> CharParser st String
end str = try $
  string "\\end" *> between (char '{') (char '}') (string str)
  
-- string delimited by braces
braced :: CharParser st String
braced = between (char '{') (char '}') (many $ noneOf "}")

-- a non-empty string of characters other than \ $ { }
text :: CharParser st Item
text = Text <$> many1 (noneOf "\\${}")
  
test :: String
test = 
  let file = "\\begin{foo}abc\\begin{bar}def\\end{bar}\\begin{baz}ghi\\end{baz}\\end{foo}" 
  in
    case parse item "Experiment" file of
        Left err -> "Error: " ++ show err
        Right doc -> show doc
