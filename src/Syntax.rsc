module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form = "form" Id name Block body;

syntax Block = @Foldable bracket "{" Question* qs "}";

syntax Guard = @Foldable bracket "(" Expr ")";

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = Str Id ":" Type ("=" Expr)?
  | Block
  | "if" Guard Question ("else" Question)?;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ Reserved
  | Literal
  | bracket "(" Expr ")"
  > right ("!" | "+" | "-") Expr
  > left Expr ("*" | "/") Expr
  > left Expr ("+" | "-") Expr
  > left Expr ("!=" | "==") Expr
  > left Expr ("\<" | "\>" | "\<=" | "\>=") Expr
  > left Expr ("&&" | "||") Expr;

syntax Type = Primitive;
syntax Literal = Bool | Int | Str;

lexical Bool = BoolValue;
lexical Int = [0-9]+;
lexical Str = "\"" ![\"]* "\"";

keyword Primitive = "boolean" | "integer" | "string";
keyword BoolValue = "true" | "false";

keyword Reserved = "if" | "then" | BoolValue | Primitive;