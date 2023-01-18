module CST2AST

import Syntax;
import AST;
import String;

import ParseTree; // VSCode says it is unused, but it breaks when you remove it
void _() {parse(#Id,"");} // Prevents said unused warning, there should be an annotation for that

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form("<f.name>", cst2ast(f.body), src = f.src); 
}

ABlock cst2ast(Block b) {
  return block([cst2ast(q) | Question q <- b.questions], src = b.src);
}

AQuestion cst2ast(Question q) {
  switch (q) {
    case (Question)`<Str a> <Id var> : <Type t>` :
      return question(cst2ast(a), cst2ast(var), cst2ast(t), none(), src = q.src);
    case (Question)`<Str a> <Id var> : <Type t> = <Expr expr>` :
      return question(cst2ast(a), cst2ast(var), cst2ast(t), cst2ast(expr), src = q.src);
    case (Question)`<Block b>` :
      return nested(cst2ast(b), src = q.src);
    case (Question)`if ( <Expr e> ) <Question then>`:
      return ifthen(cst2ast(e), cst2ast(then), nested(block([])), src = q.src);
    case (Question)`if ( <Expr e> ) <Question then> else <Question other>`:
      return ifthen(cst2ast(e), cst2ast(then), cst2ast(other), src = q.src);

    default: throw "Unimplemented question type <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(cst2ast(x), src=x.src);
    case (Expr)`<Int i>`: return var(cst2ast(i), src=i.src);
    case (Expr)`<Bool b>`: return var(cst2ast(b), src=b.src);

    case (Expr)`( <Expr e> )`: return cst2ast(e);

    case (Expr)`! <Expr e>`: return not(cst2ast(e), src=e.src);
    case (Expr)`- <Expr e>`: return neg(cst2ast(e), src=e.src);
    case (Expr)`+ <Expr e>`: return pos(cst2ast(e), src=e.src);

    case (Expr)`<Expr l> * <Expr r>`: return mul(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> / <Expr r>`: return div(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> + <Expr r>`: return add(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> - <Expr r>`: return sub(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> == <Expr r>`: return yeq(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> != <Expr r>`: return neq(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> \< <Expr r>`: return lt(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> \> <Expr r>`: return gt(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> \<= <Expr r>`: return lte(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> \>= <Expr r>`: return gte(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> && <Expr r>`: return and(cst2ast(l), cst2ast(r), src=e.src);
    case (Expr)`<Expr l> || <Expr r>`: return or(cst2ast(l), cst2ast(r), src=e.src);
    
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
    case (Type)`boolean`: return tbool();
    case (Type)`integer`: return tint();
    case (Type)`string`: return tstr();

    default: throw "Invalid type: <t>";
  }
}

AId cst2ast(Id i) {
  return id("<i>", src=i.src);
}

AStr cst2ast(Str name) {
  return string("<name>"[1..-1], src=name.src);
}

AInt cst2ast(Int v) {
  return integer(toInt("<v>"), src=v.src);
}

ABool cst2ast(Bool b) {
  return boolean("<b>" == "true", src=b.src);
}