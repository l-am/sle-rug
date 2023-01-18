module Tester

import Syntax;
import AST;
import CST2AST;
import Resolve;
import Check;

import ParseTree;
import IO;

AForm getExampleFile(str name) {
  // Resolving makes the links in VSCode clickable when printing
  return cst2ast(parse(#start[Form], resolveLocation(|cwd:///../examples/<name>.myql|)));
}

Log checkForm(AForm form) {
  return check(form, collect(form), resolve(form)[2]);
}

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
test bool checkCyclicFile() = (error(_, _) <- checkForm(getExampleFile("cyclic")));
test bool checkErrorsFile() {
  Log log = checkForm(getExampleFile("errors"));
  list[str] errs = [
    "Operands have different types",
    "Use of undeclared variable"
  ];
  bool r = true;
  for (str err <- errs) if(!(error(err, _) <- log)) {
    println("Should error \"<err>\".");
    r = false;
  }
  return r;
}
test bool checkNothingFile() = checkForm(getExampleFile("nothing")) == {};
test bool checkEverythingFile() {
  Log log = checkForm(getExampleFile("errors"));
  list[str] errs = [
    "Operands have different types",
    "Use of undeclared variable"
  ];
  bool r = true;
  for (str err <- errs) if(!(error(err, _) <- log)) {
    println("Should error \"<err>\".");
    r = false;
  }
  return r;
}

void printLog(Log log) {
  for (Message m <- log) {
    println(m);
  }
}

void main() {
  for (f <- ["binary", "cyclic", "empty", "errors", "tax", "everything", "nothing"]) {
    println("Log for <f>.myql:");
    printLog(checkForm(getExampleFile(f)));
    println();
  }
}