module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s);

// The value environment
alias VEnv = map[str name, Value v];

// Modeling user input
data Input = input(str question, Value v);

map[AType, Value] defaults = (
  tint(): vint(0), 
  tbool(): vbool(false),
  tstr(): vstr("")
);

// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  return (i.name: defaults[t] | /question(_, AId i, AType t, _) := f);
}

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = (venv | eval(q, inp, it) | AQuestion q <- f.body.questions);
  }
}

// evaluate conditions for branching,
// evaluate inp and computed questions to return updated VEnv
VEnv eval(nested(ABlock b), Input inp, VEnv venv)
    = (venv | eval(q, inp, it) | AQuestion q <- b.questions);
VEnv eval(ifthen(AExpr e, AQuestion then, AQuestion other), Input inp, VEnv venv)
    = eval(eval(e, venv).b ? then : other, inp, venv);
VEnv eval(question(AStr s, AId i, AType t, AExpr e), Input inp, VEnv venv) 
    = venv + (i.name: e == none() ? (i.name == inp.question ? inp.v : venv[i.name]) : eval(e, venv));

int MAX64 = (1 | it * 2 | _ <- [1..63]) - 1;

Value eval(AExpr e, VEnv venv) {
  list[Value] r = [eval(x, venv) | AExpr x <- e];
  switch (e) {
    case ref(id(str x)): return venv[x];
    case var(boolean(str b)): return vbool(b == "true");
    case var(integer(int i)): return vint(i);
    case var(string(str s)): return vstr(s);
    case not(_): return vbool(!r[0].b);
    case neg(_): return vint(-r[0].n);
    case pos(_): return vint(r[0].n);
    case mul(_, _): return vint(r[0].n * r[1].n);
    case div(_, _): return vint(r[1].n == 0 ? MAX64 : (r[0].n /  r[1].n));
    case add(_, _): return vint(r[0].n + r[1].n);
    case sub(_, _): return vint(r[0].n - r[1].n);
    case lt(_, _): return vbool(r[0].n < r[1].n);
    case gt(_, _): return vbool(r[0].n > r[1].n);
    case lte(_, _): return vbool(r[0].n <= r[1].n);
    case gte(_, _): return vbool(r[0].n >= r[1].n);
    case yeq(_, _): return vbool(r[0] == r[1]);
    case neq(_, _): return vbool(r[0] != r[1]);
    case and(_, _): return vbool(r[0].b && r[1].b);
    case or(_, _): return vbool(r[0].b || r[1].b);
    
    default: throw "Unsupported expression <e>";
  }
}