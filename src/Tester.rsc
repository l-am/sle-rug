module Tester

import Syntax;
import AST;
import CST2AST;
import Resolve;
import Check;
import Eval;
import Compile;
import Transform;

import ParseTree;
import IO;


// Resolving makes the links in VSCode clickable when printing
start[Form] getCST(str name) = parse(#start[Form], resolveLocation(|cwd:///../examples/<name>.myql|));

AForm getAST(str name) = cst2ast(getCST(name));

Log checkForm(AForm form) = check(form, collect(form), resolve(form)[2]);

bool hasToError(str err, Log log) {
  bool r = (error(err, _) <- log);
  if (!r) println("Has to error \"<err>\".");
  return r;
}

VEnv eval(AForm form, list[Input] inp) = (initialEnv(form) | eval(form, i, it) | Input i <- inp);

test bool usesAndDefsIsCorrect() {
  RefGraph rg = resolve(getAST("tax"));
  set[str] uses = {x.name | x <- rg.uses};
  set[str] defs = {x.name | x <- rg.defs};
  return uses == {"hasSoldHouse","sellingPrice","privateDebt"}
    && defs == {"hasBoughtHouse","hasMaintLoan","hasSoldHouse","sellingPrice","privateDebt","valueResidue"};
}

test bool checkEmptyFile() = checkForm(getAST("empty")) == {};

test bool checkTaxFile() = checkForm(getAST("tax")) == {};

test bool checkBinaryFile() = !(error(_, _) <- checkForm(getAST("binary")));

test bool checkCyclicFile() {
  Log log = checkForm(getAST("errors"));
  return (true | it && hasToError(err, log) | str err <- [
    // No stuff for cyclic errors (yet)
  ]);
}

test bool checkErrorsFile() {
  Log log = checkForm(getAST("errors"));
  return (true | it && hasToError(err, log) | str err <- [
    "Operands have different types",
    "Use of undeclared variable"
  ]);
}

test bool checkNothingFile() = checkForm(getAST("nothing")) == {};

test bool checkEverythingFile() {
  Log log = checkForm(getAST("everything"));
  return (true | it && hasToError(err, log) | err <- [
    "Operands have different types",
    "Use of undeclared variable",
    "Conflicting duplicate question variable",
    "Operator cannot be used on tbool()",
    "Conflicing types. Expected tint(), received tbool().",
    "Conflicing types. Expected tbool(), received tint()."
  ]);
}

test bool testEvalDefault() {
  VEnv v = eval(getAST("tax"), []);
  return v["sellingPrice"] == vint(0) && v["hasMaintLoan"] == vbool(false);
}

test bool testEvalSetValid() {
  return eval(getAST("tax"), [
    input("hasMaintLoan",vbool(true))
  ])["hasMaintLoan"] == vbool(true);
}

test bool testEvalSetInvalid() {
  return eval(getAST("tax"), [
    input("sellingPrice",vint(123))
  ])["sellingPrice"] == vint(0);
}

test bool testEvalSetValidAfter() {
  return eval(getAST("tax"), [
    input("hasSoldHouse",vbool(true)),
    input("sellingPrice",vint(123))
  ])["sellingPrice"] == vint(123);
}

test bool testEvalCalculate() {
  return eval(getAST("tax"), [
    input("hasSoldHouse",vbool(true)),
    input("sellingPrice",vint(123)),
    input("privateDebt",vint(456))
  ])["valueResidue"] == vint(-333);
}

test bool testEvalOverloadedEquals1() {
  return eval(getAST("nothing"), [
    input("bool3",vbool(true))
  ])["bool3"] == vbool(true);
}

test bool testEvalOverloadedEquals2() {
  return eval(getAST("nothing"), [
    input("bool2",vbool(true)),
    input("bool3",vbool(true))
  ])["bool3"] == vbool(false);
}

test bool testEvalOverloadedEquals3() {
  return eval(getAST("nothing"), [
    input("int3",vint(1))
  ])["int3"] == vint(1);
}

test bool testEvalOverloadedEquals4() {
  return eval(getAST("nothing"), [
    input("int2",vint(1)),
    input("int3",vint(1))
  ])["int3"] == vint(0);
}

test bool testRename() {
  start[Form] cst = getCST("tax");
  AForm ast = cst2ast(cst);
  for (/AId t <- ast) {
    list[str] result = [i.name | /AId i <- cst2ast(rename(cst, t.src, "RENAMED", resolve(ast)[2]))];
    if (t.name in result) return false;
  }
  return true;
}

void printLog(Log log) {
  for (Message m <- log) {
    println(m);
  }
}

void manualErrors() {
  for (f <- ["binary", "cyclic", "empty", "errors", "tax", "everything", "nothing"]) {
    println("Log for <f>.myql:");
    printLog(checkForm(getAST(f)));
    println();
  }
}

void manualEval() {
  println("Eval test:");
  AForm tax = getAST("tax");
  VEnv venv = initialEnv(tax);
  println(eval(getAST("tax"), []));
  println(eval(getAST("tax"), [
    input("hasSoldHouse",vbool(true)),
    input("sellingPrice",vint(123)),
    input("privateDebt",vint(456)),
    input("hasSoldHouse",vbool(false)),
    input("sellingPrice",vint(789)) // expected to be ignored
  ]));
}

void manualCompile() {
  compile(getAST("binary"));
  compile(getAST("tax"));
}

void manualFlatten() {
  iprintln(flatten(getAST("tax")));
}

void manualRename() {
  start[Form] cst = getCST("tax");
  AForm ast = cst2ast(cst);
  iprintln(ast);
  println([i.name | /AId i <- ast]);
  loc renameMe = [i.src | /AId i <- ast][2];
  println([i.name | /AId i <- cst2ast(rename(cst, renameMe, "RENAMED", resolve(ast)[2]))]);
}

// Some quick output for manual testing
void main() {
  manualErrors();
  manualEval();
  manualCompile();
  manualFlatten();
  manualRename();
}