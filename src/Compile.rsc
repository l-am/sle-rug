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
      title([text(f.name)]),
      style([text("div {
                  '  margin: 10px 20px;
                  '}")])
    ]),
    body([
      h1([text(f.name)]),
      form(
        [e | AQuestion q <- f.body.questions, HTMLElement e <- q2html(q)]
        + input()[\type="button"][\value="Submit <f.name>"]
      ),
      script([])[src="./<f.src[extension="js"].file>"]
    ])
  ]);

list[HTMLElement] q2html(nested(block([]))) = [];
list[HTMLElement] q2html(nested(ABlock b)) = ([] | it + q2html(q) | AQuestion q <- b.questions);
list[HTMLElement] q2html(ifthen(AExpr e, AQuestion then, AQuestion other))
  = [div(q2html(then))[id="if_<e.src.offset>"][class="conditional"], 
    div(q2html(other))[id="else_<e.src.offset>"][class="conditional"]];
list[HTMLElement] q2html(question(AStr s, AId i, AType t, AExpr e)) {
  HTMLElement inp = elTypes[t][id=i.name][class="question"];
  if (e != none()) inp.disabled = "";
  return [div([p([text(s.name)]), inp])];
}

str form2js(AForm f) {
  map[AType, str] t2js = (
    tint(): "0",
    tbool(): "false",
    tstr(): "\"\""
  );
  return "function subId(id, parent) {return (parent||document).getElementById(id);}
         'function subTag(tag, parent) {return [...(parent||document).getElementsByTagName(tag)];}
         'function subClass(cls, parent) {return [...(parent||document).getElementsByClassName(cls)];}
         '
         'const form = subTag(`form`)[0];
         'const v = {<
          for (/question(_, AId i, AType t, AExpr e) <- f, e == none()) {>
         '  <i.name>: <t2js[t]>,<
          }><
          for (/question(_, AId i, _, AExpr e) <- f, e != none()) {>
         '  get <i.name>(){return <expr2js(e, prefix="this.")>},<
          }><
          for (/ifthen(AExpr e, _, _) <- f) {>
         '  get $<e.src.offset>(){return <expr2js(e, prefix="this.")>},<
          }>
         '};
         '
         'function update(e) {
         '  let t = e?.target;
         '  if(t) v[t.id] = t[{checkbox: `checked`, number: `valueAsNumber`, text: `value`}[t.type]];
         '  subClass(`conditional`, form).forEach(x =\> {
         '    let [type, id] = x.id.split(`_`);
         '    x.style.display = v[`$${id}`] == (type == `if`) ? `block` : `none`;
         '  });
         '  subClass(`question`, form).forEach(x =\> {
         '    x[x.type == `checkbox` ? `checked` : `value`] = v[x.id];
         '  });
         '}
         '
         'subTag(`input`, form).forEach(x =\> x.onchange = update);
         'update();
         ";
}

str expr2js(AExpr e, str prefix = "") {
  list[str] r = [expr2js(x, prefix=prefix) | AExpr x <- e];
  switch (e) {
    case ref(id(str x)): return "<prefix><x>";
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