module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|) = form(str name, ABlock body); 

data ABlock(loc src = |tmp:///|) = block(list[AQuestion] questions);

data AQuestion(loc src = |tmp:///|)
  = question(AStr text, AId variable, AType datatype, AExpr expr)
  | nested(ABlock b)
  | ifthen(AExpr expr, AQuestion then, AQuestion other); 

data AExpr(loc src = |tmp:///|)
  = var(ABool boolean)
  | var(AInt integer)
  | var(AStr string)
  | ref(AId reference)
  | not(AExpr expr)
  | neg(AExpr expr)
  | pos(AExpr expr)
  | mul(AExpr l, AExpr r)
  | div(AExpr l, AExpr r)
  | add(AExpr l, AExpr r)
  | sub(AExpr l, AExpr r)
  | lt(AExpr l, AExpr r)
  | gt(AExpr l, AExpr r)
  | lte(AExpr l, AExpr r)
  | gte(AExpr l, AExpr r)
  | yeq(AExpr l, AExpr r)
  | neq(AExpr l, AExpr r)
  | and(AExpr l, AExpr r)
  | or(AExpr l, AExpr r)
  | none();

data AType(loc src = |tmp:///|)
  = tbool()
  | tint()
  | tstr()
  | tunknown();

data AId(loc src = |tmp:///|)
  = id(str name);

data AStr(loc src = |tmp:///|)
  = string(str name);

data AInt(loc src = |tmp:///|)
  = integer(int val);

data ABool(loc src = |tmp:///|)
  = boolean(str val);