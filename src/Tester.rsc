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

void printMsgs(str name) {
  Log l = checkForm(getAST(name));
  for (warning(str m, _) <- l) println("[<name>] warning\>\> <m>");
  for (error(str m, _) <- l) println("[<name>] error\>\> <m>");
}
bool expectMsgs(str name, set[str] msgs) {
  set[str] r = {m | /str m <- checkForm(getAST(name))};
  for (str m <- r - msgs) println("[<name>] \>\> <m>");
  for (str m <- msgs - r) println("[<name>] \<\< <m>");
  return r == msgs;
}

VEnv eval(AForm form, list[Input] inp) = (initialEnv(form) | eval(form, i, it) | Input i <- inp);

test bool usesAndDefsIsCorrect() {
  RefGraph rg = resolve(getAST("tax"));
  set[str] uses = {x.name | x <- rg.uses};
  set[str] defs = {x.name | x <- rg.defs};
  return uses == {"hasSoldHouse","sellingPrice","privateDebt"}
    && defs == {"hasBoughtHouse","hasMaintLoan","hasSoldHouse","sellingPrice","privateDebt","valueResidue"};
}

test bool checkEmptyFile() = expectMsgs("empty", {});
test bool checkTaxFile() = expectMsgs("tax", {});
test bool checkBinaryFile() = expectMsgs("binary", {});
test bool checkCyclicFile() = expectMsgs("cyclic", {
  "Variable (sellingPrice) is not reachable",
  "Variable (valueResidue) is not reachable",
  "Variable (privateDebt) is not reachable",
  "Variable (hasSoldHouse) is not reachable"
});
test bool checkErrorsFile()  = expectMsgs("errors", {
  "Operands have different types",
  "Use of undeclared variable"
});
test bool checkNothingFile() = expectMsgs("nothing", {});
test bool checkEverythingFile() = expectMsgs("everything", {
  "Operands have different types",
  "Use of undeclared variable",
  "Conflicting duplicate question variable",
  "Operator cannot be used on tbool()",
  "Conflicing types. Expected tint(), received tbool().",
  "Conflicing types. Expected tbool(), received tint().",
  "Variable (bool2) is not reachable",
  "Variable (bool3) is not reachable",
  "Variable (bool4) is not reachable",
  "Variable (int6) is not reachable"
});

test bool testEvalDefaults() {
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

void manualErrors() {
  println("\nErrors test:");
  for (f <- ["binary", "cyclic", "empty", "errors", "tax", "everything", "nothing"]) {
    printMsgs(f);
  }
}

void manualEval() {
  println("\nEval test:\nShould have 123 and 456, not 789");
  AForm tax = getAST("tax");
  VEnv venv = initialEnv(tax);
  println(eval(getAST("tax"), []));
  println(eval(getAST("tax"), [
    input("hasSoldHouse",vbool(true)),
    input("sellingPrice",vint(123)),
    input("privateDebt",vint(456)),
    input("hasSoldHouse",vbool(false)),
    input("sellingPrice",vint(789))
  ]));
}

void manualCompile() {
  println("\nCompile test:");
  for (f <- ["binary", "tax", "nothing"]) {
    compile(getAST(f));
    println("Compiled [<f>]");
  }
}

void manualFlatten() {
  println("\nFlatten test:");
  for(/ifthen(AExpr e, question(_, AId i, _, _), nested(block([]))) <- flatten(getAST("tax"))) {
    println("if (<expr2js(e, ())>) <i.name>");
  }
}

void manualRename() {
  println("\nRename test:");
  start[Form] cst = getCST("tax");
  AForm ast = cst2ast(cst);
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