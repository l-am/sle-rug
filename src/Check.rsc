module Check

import AST;
import Resolve;
import Message;

// I don't see the need to convert AType to something else

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, AType t];

alias Log = set[Message];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  return {<i.src, i.name, q.name, t> | /question(AStr q, AId i, AType t, _) := f};
}

Log check(AForm f, TEnv tenv, UseDef useDef) {
  return {m | /AQuestion q := f, m <- check(q, tenv, useDef)};
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
Log check(question(AStr q, AId i, AType t, AExpr e), TEnv tenv, UseDef useDef) {
  bool sameVarDiffLabel = any(<_, n, l, _> <- tenv, n == i.name, l != q.name);
  bool sameVarDiffType = any(<_, n, _, x> <- tenv, n == i.name, t != x);
  bool sameLabelDiffVar = any(<_, n, l, _> <- tenv, n != i.name, l == q.name);
  
  return {warning("Duplicate question variable", q.src) | sameVarDiffLabel && !sameVarDiffType}
    + {error("Conflicting duplicate question variable", q.src) | sameVarDiffType}
    + {warning("Duplicate question label", q.src) | sameLabelDiffVar}
    + check(e, t, tenv, useDef);
}

Log check(nested(_), TEnv tenv, UseDef useDef) = {};

Log check(ifthen(AExpr e, _, _), TEnv tenv, UseDef useDef) = check(e, tbool(), tenv, useDef);

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
Log check(AExpr e, AType expected, TEnv tenv, UseDef useDef) {
  if (e == none()) return {};
  bool diffTypes = false;
  <myType, mySubTypes> = opType(e, tenv, useDef);
  AType subType = tunknown();

  for (AExpr x <- e) {
    AType a = typeOf(x, tenv, useDef);
    if (subType != tunknown() && a != subType) diffTypes = true;
    subType = a;
  }

  return {*check(x, tunknown(), tenv, useDef) | AExpr x <- e}
    + {error("Operands have different types", e.src) | diffTypes}
    + {error("Operator cannot be used on <subType>", e.src) | !diffTypes && subType notin mySubTypes+tunknown()}
    + {error("Use of undeclared variable", x.src) | ref(AId x) := e, useDef[x.src] == {}}
    + {error("Conflicing types. Expected <expected>, received <myType>.", e.src) | expected notin {myType, tunknown()} && myType != tunknown()};
}

AType typeOf(AExpr e, TEnv tenv, UseDef useDef) = opType(e, tenv, useDef)[0];

tuple[AType, set[AType]] opType(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)): if (<u, loc d> <- useDef, <d, _, _, AType t> <- tenv) return <t, {}>;
    case var(ABool _): return <tbool(), {}>;
    case var(AInt _): return <tint(), {}>;
    case var(AStr _): return <tstr(), {}>;
    case not(_): return <tbool(), {tbool()}>;
    case neg(_): return <tint(), {tint()}>;
    case pos(_): return <tint(), {tint()}>;
    case mul(_, _): return <tint(), {tint()}>;
    case div(_, _): return <tint(), {tint()}>;
    case add(_, _): return <tint(), {tint()}>;
    case sub(_, _): return <tint(), {tint()}>;
    case lt(_, _): return <tbool(), {tint()}>;
    case gt(_, _): return <tbool(), {tint()}>;
    case lte(_, _): return <tbool(), {tint()}>;
    case gte(_, _): return <tbool(), {tint()}>;
    case yeq(_, _): return <tbool(), {tbool(), tint()}>;
    case neq(_, _): return <tbool(), {tbool(), tint()}>;
    case and(_, _): return <tbool(), {tbool()}>;
    case or(_, _): return <tbool(), {tbool()}>;
  }
  return <tunknown(), {}>; 
}
