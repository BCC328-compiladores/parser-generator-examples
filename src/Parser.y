-- basic haskell block

{
module Parser (parser) where

import Lexer
}

-- definition of the name of the generated parser function

%name      expParser

-- definition of the token type

%tokentype {Token}

-- definition of the error function

%error     {parseError}

-- definition of the tokens

%token
     int  {TNumber $$}
     var  {TVar $$}
     '+'  {TAdd}
     '*'  {TMul}
     '('  {TLParen}
     ')'  {TRParen}

%%

-- definition of the grammar rules and its associated actions.

Expr   : Term '+' Expr   {Add $1 $3}
       | Term            {$1}

Term   : Factor '*' Term {Mul $1 $3}
       | Factor          {$1}

Factor : int           {Number $1}
       | var           {Var $1}
       | '(' Expr ')'  {$2}

-- more haskell code

{
parseError :: [Token] -> a
parseError _ = error "Parse error!"

data Exp
  = Number Int
  | Var String
  | Add Exp Exp
  | Mul Exp Exp
  deriving Show

parser :: String -> Exp
parser = expParser . lexer
}
