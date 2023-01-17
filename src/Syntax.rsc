module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form = "form" Id name Block body;

syntax Block = @Foldable bracket "{" Question* questions "}";

syntax Guard = @Foldable bracket "(" Expr expr ")";

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = ask: Str text Id var ":" Type t ("=" Expr val)?
  | qblock: Block b
  | ifstmt: "if" Guard guard Block then ("else" Block other)?;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = var: Id \ Reserved
  | Bool
  | Int
  | Str
  | bracket "(" Expr ")"
  > right (not: "!" Expr
        | neg: "-" Expr
        | pos: "+" Expr)
  > left (mul: Expr l "*"   Expr r
        | div: Expr l "/"   Expr r)
  > left (add: Expr l "+"   Expr r
        | sub: Expr l "-"   Expr r)
  > left (eq:  Expr l "=="  Expr r
        | neq: Expr l "!="  Expr r)
  > left (lt:  Expr l "\<"  Expr r
        | gt:  Expr l "\>"  Expr r
        | lte: Expr l "\<=" Expr r
        | gte: Expr l "\>=" Expr r)
  > left (and: Expr l "&&"  Expr r
        | or:  Expr l "||"  Expr r);
  
syntax Type = Primitive;

lexical Primitive = "boolean" | "integer" | "string";

lexical Bool = "true" | "false";
lexical Int = [0-9]+;
lexical Str = [\"] ![\"]* [\"];

keyword Reserved = "if" | "then" | Bool | Primitive;

