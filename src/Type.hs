{-# LANGUAGE OverloadedStrings #-}

module Type where

import Name

data Lit
  = LitStr String
  | LitNum Double
  | LitBool Bool
  | LitNull
  | LitUndef
  | LitSymbol Name
  | LitObject [(Name, Expr)]
  | LitArray [Expr]
  deriving (Show)

{- ORMOLU_DISABLE -}
data Expr
  = Exprs [Expr]
--   | Break
--   | Catch
--   | Continue
--   | DoWhile
  | For Expr Expr Expr Expr
--   | ForIn
--   | Function
  | IfElse Expr Expr Expr
  | Return Expr
--   | Switch
--   | Throw
--   | Try
  | Var Name Expr
--   | While
  | Elit Lit
  | Fun [Name] Expr    -- function defined
  | AppFun Expr [Expr]   -- functi on App
  | Assignment Name Expr  -- "a = 1"
  | BuildInFunction ([Expr] -> IO (Either EvalError Expr))
  | Skip  -- if any args is Skip, then skip next cal , write result to Skip
  -- e: a.b.c.d.e
  | ObjectGet Name [Name]
{- ORMOLU_ENABLE -}

isSkip :: Expr -> Bool
isSkip Skip = True
isSkip _ = False

isBuildIn :: Expr -> Bool
isBuildIn (BuildInFunction _) = True
isBuildIn _ = False

isFun :: Expr -> Bool
isFun (Fun _ _) = True
isFun _ = False

data EvalError
  = SymbolNotFind
  | ArgsNotMatch
  | RefTypeError
  | AppExprError
  | AddTypeError [Expr]
  | LessTypeError [Expr]
  | NeverHappened
  | VarNotDefined String
  | UnSpportAppFun String
  | UnallocatedAddr
  | UninitializedAddr
  | NotObject Name 
  | ObjectNotF Name
  | ThisPointStrangeError
  deriving (Show)

instance Show Expr where
  show (BuildInFunction fun) = " build in function"
  show (Exprs ls) = " exprs: " ++ show ls
  show (For a1 a2 a3 b) = " for: " ++ show a1 ++ show a2 ++ show a3 ++ show b
  show (Return e) = " return: " ++ show e
  show (Var name e) = " var: " ++ show name ++ show e
  show (Elit l) = " lit: " ++ show l
  show (Fun args e) = " fun: " ++ show args ++ show e
  show (AppFun e args) = " AppFun: " ++ show e ++ show args
  show (Assignment name e) = " " ++ show name ++ " = " ++ show e
  show (IfElse a b c) = "if: " ++ show a ++ show b ++ show c
  show Skip = "skip: "
  show (ObjectGet name ls) = "ObjectGet: " ++ show name ++ "  " ++ show ls

-- >>> snd <$> runEval t
-- Right  lit: LitNum 55.0
t =
  Exprs
    [ Var "a" (Elit $ LitNum 1),
      Var "b" (Elit $ LitNum 2),
      Var "c" (Elit $ LitNum 0),
      Var
        "add"
        ( Fun
            ["a1", "b1"]
            ( Exprs
                [ AppFun (Elit (LitSymbol "logger")) [Elit (LitSymbol "c")],
                  AppFun
                    (Elit (LitSymbol "logger"))
                    [ Elit (LitSymbol "i")
                    ],
                  AppFun
                    (Elit (LitSymbol "logger"))
                    [ AppFun
                        (Elit (LitSymbol "<"))
                        [Elit (LitSymbol "i"), Elit (LitNum 10)]
                    ],
                  Return $
                    AppFun
                      (Elit (LitSymbol "+"))
                      [Elit (LitSymbol "a1"), Elit (LitSymbol "b1")]
                ]
            )
        ),
      For
        (Var "i" (Elit $ LitNum 0))
        ( AppFun
            (Elit (LitSymbol "<"))
            [Elit (LitSymbol "i"), Elit (LitNum 10)]
        )
        ( Assignment "i" $
            AppFun
              (Elit (LitSymbol "+"))
              [Elit (LitSymbol "i"), Elit (LitNum 1)]
        )
        ( Exprs
            [ Assignment "c" $
                AppFun
                  (Elit (LitSymbol "add"))
                  [Elit (LitSymbol "i"), Elit (LitSymbol "c")]
            ]
        )
    ]

{-
var a = 1 ;
var b = 2 ;
var c = 0 ;

var add = function (a1,b1) {
  return (a1 + b1);
};

for (var i = 0; i < 10; i= i + 1) {
  logger(c);
  logger(i);
  logger(i < 10);
  c = add(i,c);
}

-}