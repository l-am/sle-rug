module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form = @Foldable "form" Id name "{" Question* questions "}";

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = question: Str q Id var ":" Id type ("=" Expr expr)?
  | block: "{" Question* questions "}"
  | @Foldable ifthen: "if" "(" Expr guard ")" "{" Question* then "}" ("else" "{" Question* else "}")?;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = var: Id \ Reserved
  | Bool
  | Int
  | Str
  | bracket "(" Expr ")"
  > right neg: "-" Expr
  | right pos: "+" Expr
  > left mul: Expr l "*"   Expr r
  | left div: Expr l "/"   Expr r
  > left add: Expr l "+"   Expr r
  | left sub: Expr l "-"   Expr r
  > left eq:  Expr l "=="  Expr r
  | left neq: Expr l "!="  Expr r
  > left lt:  Expr l "\<"  Expr r
  | left gt:  Expr l "\>"  Expr r
  | left lte: Expr l "\<=" Expr r
  | left gte: Expr l "\>=" Expr r
  > left and: Expr l "&&"  Expr r
  | left or:  Expr l "||"  Expr r;
  
syntax Type = Primitive;

lexical Primitive = "string" | "integer" | "boolean";

lexical Str = [\"] ![\"]* [\"];
lexical Int = [0-9]+;
lexical Bool = "true" | "false";

keyword Reserved = "if" | "then" | Bool | Primitive;

