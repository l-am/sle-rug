module Compile

import AST;
import Resolve;

import IO;
import lang::html::AST;
import lang::html::IO;

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTMLElement type and the `str writeHTMLString(HTMLElement x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

map[AType, HTMLElement] elTypes = (
  tbool(): input()[\type="checkbox"],
  tint(): input()[\type="number"][\value="0"],
  tstr(): input()[\type="text"]
);

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, writeHTMLString(form2html(f)));
}

HTMLElement form2html(AForm f)
  = html([
    head([
      title([text("Form: <f.name>")]),
      style([text("div {
                  '  margin: 10px 20px;
                  '}")])
    ]),
    body([
      h1([text("Form: <f.name>")]),
      form(
        [e | AQuestion q <- f.body.questions, HTMLElement e <- q2els(q)]
        + input()[\type="submit"][\value="Submit"]
      ),
      script([])[src="./<f.src[extension="js"].file>"]
    ])
  ]);

list[HTMLElement] q2els(nested(block([]))) = [];
list[HTMLElement] q2els(nested(ABlock b)) = ([] | it + q2els(q) | AQuestion q <- b.questions);
list[HTMLElement] q2els(ifthen(AExpr e, AQuestion then, AQuestion other))
    = [div(q2els(then))[id="cond_<e.src.offset>"], div(q2els(other))[id="cond_<e.src.offset+1>"]];
list[HTMLElement] q2els(question(AStr s, AId i, AType t, AExpr e)) {
  HTMLElement inp = elTypes[t][id=i.name];
  if (e != none()) inp.disabled = "";
  return [div([
    p([text(s.name)]),
    inp
  ])];
}

str form2js(AForm f) {
  map[str, AExpr] fm = (i.name:e | /question(_, AId i, _, AExpr e) <- f, e != none());
  return "const tmap = {checkbox: \"checked\", number: \"valueAsNumber\", text: \"value\"};
         'function el(s) {return document.getElementById(s);}
         'function et(t, p) {return [...(p||document).getElementsByTagName(t)];}
         'function gv(s) {let t = el(s); return t[tmap[t.type]]}
         'function sv(s, v) {let t = el(s); t[tmap[t.type]] = v; t.value = v;}
         'function sh(n, b) {
         '  let s = [el(`cond_${n}`), el(`cond_${n+1}`)];
         '  if (b) s = [s[1],s[0]];
         '  s[0].style.display = `none`;
         '  s[1].style.display = `block`;
         '}
         'function update() {
         '  <for (/ifthen(AExpr e, _, _) <- f) {>sh(<e.src.offset>, <expr2js(e, fm)>);<}>
         '  <for (/question(_, AId i, _, AExpr e) <- f, e != none()) {>sv(\"<i.name>\", <expr2js(e, fm)>);<}>
         '  
         '}
         '
         'et(\"input\", et(\"form\")[0]).forEach(x =\> x.onclick = update);
         'update();
         ";
}

str expr2js(AExpr e, map[str, AExpr] fm) {
  list[str] r = [expr2js(x, fm) | AExpr x <- e];
  switch (e) {
    case ref(id(str x)): return (fm[x] ? none() != none()) ? expr2js(fm[x], fm) : "gv(\"<x>\")";
    case var(boolean(bool b)): return "<b>";
    case var(integer(int i)): return "<i>";
    case var(string(str s)): return "\"<s>\"";
    case not(_): return "(!<r[0]>)";
    case neg(_): return "(-<r[0]>)";
    case pos(_): return "(+<r[0]>)";
    case mul(_, _): return "(<r[0]> * <r[1]>)";
    case div(_, _): return "(<r[0]> / <r[1]>)";
    case add(_, _): return "(<r[0]> + <r[1]>)";
    case sub(_, _): return "(<r[0]> - <r[1]>)";
    case lt(_, _): return "(<r[0]> \< <r[1]>)";
    case gt(_, _): return "(<r[0]> \> <r[1]>)";
    case lte(_, _): return "(<r[0]> \<= <r[1]>)";
    case gte(_, _): return "(<r[0]> \>= <r[1]>)";
    case yeq(_, _): return "(<r[0]> == <r[1]>)";
    case neq(_, _): return "(<r[0]> != <r[1]>)";
    case and(_, _): return "(<r[0]> && <r[1]>)";
    case or(_, _): return "(<r[0]> || <r[1]>)";
    
    default: throw "Unsupported expression <e>";
  }
}