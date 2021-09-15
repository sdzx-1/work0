{
module ScriptB.B where
import ScriptB.A as A
import Type
import qualified Data.Text as T
import Name
import qualified Data.Map as M
import ScriptB.Stack
}

%monad { E } { thenE } { returnE }
%name calc
%tokentype { Token }

%token 
  'return'   { KeyWord _ "return"}
  'if'       { KeyWord _ "if" }
  'else'     { KeyWord _ "else" }
  'while'    { KeyWord _ "while" }
  'def'      { KeyWord _ "def" }

  ';'  { Separators _ ";" }
  '+'  { Separators _ "+" }
  '('  { Separators _ "(" }
  ')'  { Separators _ ")" }
  '<'  { Separators _ "<" }
  '{'  { Separators _ "{" }
  '}'  { Separators _ "}" }
  ','  { Separators _ "," }
  '='  { Separators _ "=" }
  '.'  { Separators _ "." }
  ':'  { Separators _ ":" }
  '\{' { LayoutStart }
  '\;' { LayoutStep }
  '\}' { LayoutEnd }
  
  num    { Number _ $$ }
  string { String _ $$ }
  var    { A.Var  _ $$ }

%% 

sep(a, s) :: { [a] }
  : {- empty -} { []         }
  | sep1(a, s)  { reverse $1 }

sep1(a, s) :: { [a] }
  : a { [$1] }
  | sep1(a, s) s a { $3 :   $1 }
  | sep1(a, s) s   { $1 }

many(a) :: { [a] }
  : {- empty -} { []       }
  | many1(a)  { reverse $1 }

many1(a) :: { [a] }
  : a { [$1] }
  | many1(a) a { $2 : $1 }

Expr0 :: { Expr }
  : '\{' sep(Expr, '\;') '\}' { Exprs $2 }
  | '\{' Expr '\}'            { $2 }

Expr :: { Expr }
  : Lit                                                    { Elit $1 }
  | 'if' Expr ':' Expr0 'else' ':' Expr0                   { IfElse $2 $4 $7 }
  | 'return' Expr                                          { Return $2 }
  | 'while' Expr ':' Expr0                                 { While $2 $4 }
  | 'def' Name '(' sep(Name, ',') ')' ':'  Expr0           { Type.Var $2 (Fun $4 $7) }
  | Expr '(' sep(Expr, ',') ')'                            { AppFun $1 $3 }
  | Expr '<' Expr                                          { AppFun ( Elit $ LitSymbol $ Name $ T.pack "<") [$1, $3] }    
  | Expr '+' Expr                                          { AppFun ( Elit $ LitSymbol $ Name $ T.pack  "+")  [$1, $3] }    
  | Name '=' Expr                                          { Assignment $1 $3 }
  
Name :: { Name }
  : var { name $ T.pack $1 }

Lit :: { Lit }
  : string { LitStr $1 }
  | num { LitNum $1 }
  | var { LitSymbol (name $ T.pack $1)  }

{

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = case m of 
    Ok a     -> k a
    Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err


runCalc :: String -> Either String Expr
runCalc s = 
   case scanner s of 
      Left err -> Left err
      Right rs -> 
        let  (_,(b,(c,d))) = runLayout rs
        in case d of 
             Left e -> Left (show c ++ " : " ++ show e)
             Right _ ->  case  calc (reverse b) of 
               Ok a -> Right a 
               Failed s -> Left s

happyError :: [Token] -> E a
happyError tks = case tks of 
    [] -> Failed "parse error: input null" 
    t:ls -> Failed $ "parse error: " ++ show (t:ls)
}