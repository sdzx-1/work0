{
module A where
import Control.Monad (forM_)

import Data.Char (ord)
import qualified Data.Bits
import Data.Word(Word8)
}


%wrapper "posn"

$digit = 0-9			-- digits
$sign = [\+ \-]


$alpha = [a-zA-Z]		-- alphabetic characters

@separators = \; | \+ | \( | \) | \< | \{ | \} | \, | \=

@keywords = var|return|function|for|if|else

tokens :-

  $white+				;
  "--".*				;
  @keywords                         { \(AlexPn _ l c) s -> KeyWord    (Posn l c) s }
  $alpha [$alpha $digit \_ \']*		{   \(AlexPn _ l c) s -> Var        (Posn l c) s }
  @separators                       { \(AlexPn _ l c) s -> Separators (Posn l c) s}
  $sign? $digit+                    { \(AlexPn _ l c) s -> Number     (Posn l c) $ read s}
  $sign? $digit+ \. $digit+         { \(AlexPn _ l c) s -> Number     (Posn l c) $ read s }
  \"($alpha)*\"                     { \(AlexPn _ l c) s -> String     (Posn l c) $ init $ tail s}




{
-- Each action has type :: String -> Token

data Posn = Posn {
  line :: Int ,
  column :: Int
} deriving (Eq,Show)

-- The token type:
data Token
  = String Posn String
  | Number Posn Double
  | Separators Posn String
  | KeyWord Posn String
  | Var Posn String
  deriving (Eq,Show)

main = do
  s <- return "var a = 1;"
  print (alexScanTokens s)

}