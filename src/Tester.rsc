module Tester

import Syntax;
import AST;
import CST2AST;
import Resolve;

import ParseTree;
import IO;

// Shows a tree and tells you the uses and defs in test.myql
test bool usesAndDefsIsCorrect() {
    loc s = resolveLocation(|cwd:///../examples/tax.myql|); // resolving makes them clickable in VSCode
    start[Form] a = parse(#start[Form], s);
    AForm af = cst2ast(a);
    RefGraph rg = resolve(af);
    set[str] uses = {x.name | x <- rg.uses};
    set[str] defs = {x.name | x <- rg.defs};
    return uses == {"hasSoldHouse","sellingPrice","privateDebt"}
        && defs == {"hasBoughtHouse","hasMaintLoan","hasSoldHouse","sellingPrice","privateDebt","valueResidue"};
}