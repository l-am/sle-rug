module Tester

import Syntax;
import AST;
import CST2AST;
import Resolve;
import Check;
import Eval;

import ParseTree;
import IO;

AForm getExampleFile(str name) {
  // Resolving makes the links in VSCode clickable when printing
  return cst2ast(parse(#start[Form], resolveLocation(|cwd:///../examples/<name>.myql|)));
}

Log checkForm(AForm form) {
  return check(form, collect(form), resolve(form)[2]);
}

bool hasToError(str err, Log log) {
  bool r = (error(err, _) <- log);
  if (!r) println("Has to error \"<err>\".");
  return r;
}

VEnv eval(AForm form, list[Input] inp) = (initialEnv(form) | eval(form, i, it) | Input i <- inp);

test bool usesAndDefsIsCorrect() {
  RefGraph rg = resolve(getExampleFile("tax"));
  set[str] uses = {x.name | x <- rg.uses};
  set[str] defs = {x.name | x <- rg.defs};
  return uses == {"hasSoldHouse","sellingPrice","privateDebt"}
    && defs == {"hasBoughtHouse","hasMaintLoan","hasSoldHouse","sellingPrice","privateDebt","valueResidue"};
}

test bool checkEmptyFile() = checkForm(getExampleFile("empty")) == {};
test bool checkTaxFile() = checkForm(getExampleFile("tax")) == {};
test bool checkBinaryFile() = !(error(_, _) <- checkForm(getExampleFile("binary")));
test bool checkCyclicFile() {
  Log log = checkForm(getExampleFile("errors"));
  return (true | it && hasToError(err, log) | err <- [
    // TODO: Cyclic errors
  ]);
}
test bool checkErrorsFile() {
  Log log = checkForm(getExampleFile("errors"));
  return (true | it && hasToError(err, log) | err <- [
    "Operands have different types",
    "Use of undeclared variable"
  ]);
}
test bool checkNothingFile() = checkForm(getExampleFile("nothing")) == {};
test bool checkEverythingFile() {
  Log log = checkForm(getExampleFile("everything"));
  return (true | it && hasToError(err, log) | err <- [
    "Operands have different types",
    "Use of undeclared variable",
    "Conflicting duplicate question variable",
    "Operator cannot be used on tbool()",
    "Conflicing types. Expected tint(), received tbool().",
    "Conflicing types. Expected tbool(), received tint()."
    // TODO: Cyclic errors
  ]);
}

test bool testEvalDefault() {
  VEnv v = eval(getExampleFile("tax"), []);
  return v["sellingPrice"] == vint(0) && v["hasMaintLoan"] == vbool(false);
}

test bool testEvalSetValid() {
  return eval(getExampleFile("tax"), [
    input("hasMaintLoan",vbool(true))
  ])["hasMaintLoan"] == vbool(true);
}

test bool testEvalSetInvalid() {
  return eval(getExampleFile("tax"), [
    input("sellingPrice",vint(123))
  ])["sellingPrice"] == vint(0);
}

test bool testEvalSetValidAfter() {
  return eval(getExampleFile("tax"), [
    input("hasSoldHouse",vbool(true)),
    input("sellingPrice",vint(123))
  ])["sellingPrice"] == vint(123);
}

test bool testEvalCalculate() {
  return eval(getExampleFile("tax"), [
    input("hasSoldHouse",vbool(true)),
    input("sellingPrice",vint(123)),
    input("privateDebt",vint(456))
  ])["valueResidue"] == vint(-333);
}

test bool testEvalOverloadedEquals1() {
  return eval(getExampleFile("nothing"), [
    input("bool3",vbool(true))
  ])["bool3"] == vbool(true);
}

test bool testEvalOverloadedEquals2() {
  return eval(getExampleFile("nothing"), [
    input("bool2",vbool(true)),
    input("bool3",vbool(true))
  ])["bool3"] == vbool(false);
}

test bool testEvalOverloadedEquals3() {
  return eval(getExampleFile("nothing"), [
    input("int3",vint(1))
  ])["int3"] == vint(1);
}

test bool testEvalOverloadedEquals4() {
  return eval(getExampleFile("nothing"), [
    input("int2",vint(1)),
    input("int3",vint(1))
  ])["int3"] == vint(0);
}

void printLog(Log log) {
  for (Message m <- log) {
    println(m);
  }
}

// Some quick output for manual testing
void main() {
  for (f <- ["binary", "cyclic", "empty", "errors", "tax", "everything", "nothing"]) {
    println("Log for <f>.myql:");
    printLog(checkForm(getExampleFile(f)));
    println();
  }

  println("Eval test:");
  AForm tax = getExampleFile("tax");
  VEnv venv = initialEnv(tax);
  println(eval(getExampleFile("tax"), []));
  println(eval(getExampleFile("tax"), [
    input("hasSoldHouse",vbool(true)),
    input("sellingPrice",vint(123)),
    input("privateDebt",vint(456)),
    input("hasSoldHouse",vbool(false)),
    input("sellingPrice",vint(789)) // should not work
  ]));
}