{
module A where
import Control.Monad (forM_)

import Data.Char (ord)
import qualified Data.Bits
import Data.Word(Word8)
}


%wrapper "basic"

$digit = 0-9			-- digits
$sign = [\+ \-]


$alpha = [a-zA-Z]		-- alphabetic characters

@separators = \; | \+ | \( | \) | \< | \{ | \} | \, | \=

@keywords = var|return|function|for|if|else

tokens :-

  $white+				;
  "--".*				;
  @keywords                         { \s -> KeyWord s }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }
  @separators                       { \s -> Separators s}
  $sign? $digit+                    { \s -> Number $ read s}
  $sign? $digit+ \. $digit+         { \s -> Number $ read s }
  \"($alpha)*\"                     { \s -> String $ init $ tail s}




{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = String String
  | Number Double
  | Separators String
  | KeyWord String
  | Var String
  deriving (Eq,Show)

main = do
  s <- return "var a = 1;"
  print (alexScanTokens s)

}