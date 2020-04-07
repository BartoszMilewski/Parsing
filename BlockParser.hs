module BlockParser where
  
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding (many, (<|>))

data Item =
    Text String
  | Block String [Item]
  deriving Show

type CharParser st = Parsec [Char] st

item :: CharParser st Item
item =  
  do
    blockName <- begin
    Block blockName <$> blockBody <* end blockName
  <|> text

blockBody :: CharParser st [Item]
blockBody = many item

begin :: CharParser st String
begin = try $ string "\\begin" *> braced

end :: String -> CharParser st String
end str = try $
  string "\\end" *> between (char '{') (char '}') (string str)
  
braced :: CharParser st String
braced = between (char '{') (char '}') (many $ noneOf "}")

text :: CharParser st Item
text = Text <$> many1 (noneOf "\\${}")
  
test :: String
test = 
  let file = "\\begin{foo}abc\\begin{bar}def\\end{bar}\\begin{baz}ghi\\end{baz}\\end{foo}" 
  in
    case parse item "Experiment" file of
        Left err -> "Error: " ++ show err
        Right doc -> show doc
