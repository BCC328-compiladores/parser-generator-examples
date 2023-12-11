-- Initial Haskell code block
{
module LineLex ( Token (..)
               , RangedToken (..)
               , Range (..)
               , Alex
               , runAlex
               , alexError
               , AlexPosn (..)
               , alexGetInput
               , alexMonadScan
               , alexMove
               , scanMany
               ) where

import Data.ByteString.Internal (w2c)
import Control.Monad (when)

}

-- definition of the wrapper

%wrapper "monadUserState"

-- macros for the lexer definition.
-- first character set macros

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

-- second RE macros

@identifier = $alpha[$alpha $digit]* -- identifiers
@number     = $digit+



-- token declarations.

tokens :-
<0>  $white+                 ; -- removing whitespace
<0>  @number                 { tokenInteger }
<0>  @identifier             { tokenId }
<0>  \+                      { simpleToken TAdd }
<0>  \*                      { simpleToken TMul }
<0>  \(                      { simpleToken TLParen }
<0>  \)                      { simpleToken TRParen }
<0>  "//".*                  ; -- removing line comments
<0>  "{-"                    { nestComment `andBegin` comment }
<0>  "-}"                    { \ _ _ -> alexError "Error: unexpected close comment!" }

<comment> "{-"               { nestComment }
<comment> "-}"               { unnestComment }
<comment> .                  ;
<comment> \n                 ;


-- more Haskell code

{

-- definition of the state

data AlexUserState
  = AlexUserState {
      level :: Int
    }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

-- dealing with comments

nestComment :: Action RangedToken
nestComment input len = do
  modify $ \s -> s{level = level s + 1}
  skip input len

unnestComment :: Action RangedToken
unnestComment input len = do
  state <- get
  let level' = level state - 1
  put state{level = level'}
  when (level' == 0) $
    alexSetStartCode 0
  skip input len

-- initial state for the analyser

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

-- definition of the EOF token

alexEOF :: Alex RangedToken
alexEOF
    = do
         startCode <- alexGetStartCode
         when (startCode == comment) $ alexError "Error: unclosed comment"
         (pos, _ ,_ , _) <- alexGetInput
         pure $ RangedToken EOF (Range pos pos)

-- definition for range

data Range
  = Range {
      start_ :: AlexPosn
    , stop_  :: AlexPosn
    } deriving (Eq, Show)

-- a token with a range

data RangedToken
  = RangedToken {
      token_ :: Token
    , range_ :: Range
    } deriving (Eq, Show)

-- definition of a token type

data Token
  = TNumber Int
  | TVar String
  | TAdd
  | TMul
  | TLParen
  | TRParen
  | EOF
  deriving (Eq, Show)

type Action a = AlexInput -> Int -> Alex a

-- creating ranges for tokens

mkRange :: AlexInput -> Int -> Range
mkRange (start, _, _, str) len
  = Range start stop
  where
    stop = foldl alexMove start $ take len str

-- creating an identifier for token

tokenId :: Action RangedToken
tokenId inp@(_, _, _, str) len
  = pure RangedToken {
           token_ = TVar $ take len str
         , range_ = mkRange inp len
         }

-- creating an integer token

tokenInteger :: Action RangedToken
tokenInteger inp@(_, _, _, str) len
  = pure RangedToken {
           token_ = TNumber $ read $ take len str
         , range_ = mkRange inp len
         }

-- creating a token

simpleToken :: Token -> Action RangedToken
simpleToken tk inp len
  = pure RangedToken {
           token_ = tk
         , range_ = mkRange inp len
         }

-- testing function

scanMany :: String -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if token_ output == EOF
        then pure [output]
        else (output :) <$> go
}
