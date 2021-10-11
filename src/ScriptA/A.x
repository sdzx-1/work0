{
module ScriptA.A where
import Control.Monad (forM_)

import Data.Char (ord)
import qualified Data.Bits
import Data.Word(Word8)
}


%wrapper "monad"

$digit = 0-9			-- digits
$sign = [\+ \-]


$alpha = [a-zA-Z]		-- alphabetic characters

@separators = \; | \+ | \( | \) | \< | \{ | \} | \, | \= | \.| \:

@keywords = var|return|function|for|if|else|while|break|continue

@string = [$alpha $digit $sign \: \" \']

tokens :-

  $white+				;
  "--".*				;
  @keywords                         { \(AlexPn _ l c, _, _, s) len -> return $ KeyWord    (Posn l c) $ take len s }
  $alpha [$alpha $digit \_ \']*		  { \(AlexPn _ l c, _, _, s) len -> return $ Var        (Posn l c) $ take len s }
  @separators                       { \(AlexPn _ l c, _, _, s) len -> return $ Separators (Posn l c) $ take len s}
  $sign? $digit+                    { \(AlexPn _ l c, _, _, s) len -> return $ Number     (Posn l c) $ read $ take len s}
  $sign? $digit+ \. $digit+         { \(AlexPn _ l c, _, _, s) len -> return $ Number     (Posn l c) $ read $ take len s }
  \"(@string)*\"                     { \(AlexPn _ l c, _, _, s) len -> return $ String     (Posn l c) $ init $ tail $ take len s}




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
  | EOF
  deriving (Eq,Show)

alexEOF = return EOF

scanner str = reverse <$> (runAlex str $ do 
  let loop res = 
       do 
        tok <- alexMonadScan;
        case tok of 
           EOF -> return res 
           _ -> loop $! (tok : res)
  loop [])

main = do
  s <- return "var a = 1;"
  print (scanner s)

}
