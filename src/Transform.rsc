module Transform

import Syntax;
import Resolve;
import AST;

import ParseTree; // VSCode says it is unused, but it is required

value _() = #Tree; // Suppresses said unused warning

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) = form(f.name, block(([] | it + flatten(q, var(boolean(true))) | AQuestion q <- f.body.questions))); 

list[AQuestion] flatten(nested(ABlock b), AExpr e)
  = ([] | it + flatten(q, e) | AQuestion q <- b.questions);
list[AQuestion] flatten(ifthen(AExpr g, AQuestion then, AQuestion other), AExpr e)
  = flatten(then, and(e,g)) + flatten(other, and(e, not(g)));
list[AQuestion] flatten(q:question(_, _, _, _), AExpr e)
  = [ifthen(e, q, nested(block([])))];

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
  loc l = ([def | <useOrDef, loc def> <- useDef] + useOrDef)[0];
  list[loc] r = [use | aDef <- [l], <loc use, aDef> <- useDef] + l; // get all uses
  r += [def | aUse <- [r[0]], <aUse, loc def> <- useDef]; // get all defs, as long as there is a use
  
  return visit(f) {
    case Id x => [Id] newName when x.src in r
  }
}