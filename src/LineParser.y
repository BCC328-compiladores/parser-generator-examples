-- basic haskell block

{
module LineParser where

import LineLex
}

-- definition of the name of the generated parser function

%name      exprParser

-- definition of the token type

%tokentype { RangedToken }

-- definition of the error function

%error     { parseError }

-- definition of the monad

%monad { Alex }{ >>= }{ pure }

-- definition of the lexer

%lexer { lexer }{ RangedToken EOF _ }
%expect 0

-- definition of the tokens

%token
     int  {RangedToken (TNumber _) _}
     var  {RangedToken (TVar _) _}
     '+'  {RangedToken TAdd _}
     '*'  {RangedToken TMul _}
     '('  {RangedToken TLParen _}
     ')'  {RangedToken TRParen _}

-- specifying precedences is sufficient to solve shift/reduce ambiguities.

%left '+'
%left '*'

%%

-- definition of the grammar rules and its associated actions.

Expr   : Expr '+' Expr   {Add ((info $1) <-> (info $3)) $1 $3}
Expr   : Expr '*' Expr   {Mul ((info $1) <-> (info $3)) $1 $3}
       | int             {Number (info $1) (unNumber $1)}
       | var             {Var (info $1) (unId $1)}
       | '(' Expr ')'    {$2}

-- more haskell code

{

unId :: RangedToken -> String
unId (RangedToken (TVar s) _) = s
unId _ = ""

unNumber :: RangedToken -> Int
unNumber (RangedToken (TNumber n) _) = n
unNumber _ = 0

parseError :: RangedToken -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (RangedToken -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

(<->) :: Range -> Range -> Range
(Range a1 _) <-> (Range _ b2) = Range a1 b2

data Exp a
  = Number a Int
  | Var a String
  | Add a (Exp a) (Exp a)
  | Mul a (Exp a) (Exp a)
  deriving Show

class Info a where
  info :: a -> Range

instance Info Range where
    info = id

instance Info (Exp Range) where
    info (Number a _) = a
    info (Var a _) = a
    info (Add a _ _) = a
    info (Mul a _ _) = a

instance Info RangedToken where
    info = range_

parser :: String -> Either String (Exp Range)
parser s = runAlex s exprParser
}
