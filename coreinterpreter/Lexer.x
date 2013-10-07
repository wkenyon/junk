{
module Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$upper = A-Z
$lower = a-z

tokens :-
  $white+ 				;
  "--".*				;
  case                                  { \s -> Case }
  of                                    { \s -> Of }
  let					{ \s -> Let }
  in					{ \s -> In }
  push                                  { \s -> Push }
  $digit+				{ \s -> Int (read s) }
  [\;\.\\\=\+\-\*\/\(\)\{\}]		{ \s -> Sym (head s) }
  $lower [$alpha $digit \_ \']*		{ \s -> Var s }
  $upper [$alpha $digit \_ \']*         { \s -> Cons s }

{
-- Each right-hand side has type :: String -> Token

-- The token type:
data Token
 = Cons
 | Of
 | Let
 | In
 | Push
 | Sym Char
 | Var String
 | Cons String
 | Int Int
 deriving (Eq,Show)

}
