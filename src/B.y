{
module B where
import A
import Type
import qualified Data.Text as T
import Name
}

%name calc
%tokentype { Token }

%token 
  'var'      { KeyWord "var" }
  'function' { KeyWord "function"}
  'return'   { KeyWord "return"}
  'for'      { KeyWord "for"}
  'if'       { KeyWord "if" }
  'else'     { KeyWord "else" }

  ';'  { Separators ";" }
  '+'  { Separators "+" }
  '('  { Separators "(" }
  ')'  { Separators ")" }
  '<'  { Separators "<" }
  '{'  { Separators "{" }
  '}'  { Separators "}" }
  ','  { Separators "," }
  '='  { Separators "=" }

  num    { Number $$ }
  string { String $$ }
  var    { A.Var $$ }

%% 

sep(a, s) :: { [a] }
  : {- empty -} { []         }
  | sep1(a, s)  { reverse $1 }

sep1(a, s) :: { [a] }
  : a { [$1] }
  | sep1(a, s) s a { $3 :   $1 }

many(a) :: { [a] }
  : {- empty -} { []       }
  | many1(a)  { reverse $1 }

many1(a) :: { [a] }
  : a { [$1] }
  | many1(a) a { $2 : $1 }

Expr0 :: { Expr } 
  : sep(Expr, ';') { Exprs $1}

Expr :: { Expr }
  : Lit                                                    { Elit $1 }
  | 'return' '(' Expr ')'                                  { Return $3 }
  | 'var' Name '=' Expr                                    { Type.Var $2 $4 }
  | Expr '(' sep(Expr, ',') ')'                            { AppFun $1 $3 }   
  | Expr '<' Expr                                          { AppFun ( Elit $ LitSymbol $ Name $ T.pack "<") [$1, $3] }    
  | Expr '+' Expr                                          { AppFun ( Elit $ LitSymbol $ Name $ T.pack  "+")  [$1, $3] }    
  | Name '=' Expr                                          { Assignment $1 $3 }
  | 'for' '(' Expr ';' Expr ';' Expr ')' '{' Expr0 '}'      { For $3 $5 $7 $10 }
  | 'function' '(' sep(Name, ',') ')' '{' Expr0 '}'         { Fun $3 $6 }
  | 'if' '(' Expr ')' '{' Expr0 '}' 'else' '{' Expr0 '}'    { IfElse $3 $6 $10}
  | 'function' Name '(' sep(Name, ',') ')' '{' Expr0 '}'    { Type.Var $2 (Fun $4 $7)  }

Name :: { Name }
  : var { name $ T.pack $1 }

Lit :: { Lit }
  : string { LitStr $1 }
  | num { LitNum $1 }
  | var { LitSymbol (name $ T.pack $1)  }

{
runCalc :: String -> Expr
runCalc = calc . alexScanTokens

happyError :: [Token] -> a
happyError tks = error ("Parse error " ++ show (take 20 tks) ++ " \n")
}