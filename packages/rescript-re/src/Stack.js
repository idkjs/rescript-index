// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Seq from "./Seq.js";
import * as List from "@rescript/std/lib/es6/list.js";
import * as Caml_option from "@rescript/std/lib/es6/caml_option.js";
import * as Caml_exceptions from "@rescript/std/lib/es6/caml_exceptions.js";

var Empty = /* @__PURE__ */Caml_exceptions.create("Stack.Empty");

function create(param) {
  return {
          c: /* [] */0,
          len: 0
        };
}

function clear(s) {
  s.c = /* [] */0;
  s.len = 0;
  
}

function copy(s) {
  return {
          c: s.c,
          len: s.len
        };
}

function push(x, s) {
  s.c = {
    hd: x,
    tl: s.c
  };
  s.len = s.len + 1 | 0;
  
}

function pop(s) {
  var match = s.c;
  if (match) {
    s.c = match.tl;
    s.len = s.len - 1 | 0;
    return match.hd;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function pop_opt(s) {
  var match = s.c;
  if (match) {
    s.c = match.tl;
    s.len = s.len - 1 | 0;
    return Caml_option.some(match.hd);
  }
  
}

function top(s) {
  var match = s.c;
  if (match) {
    return match.hd;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function top_opt(s) {
  var match = s.c;
  if (match) {
    return Caml_option.some(match.hd);
  }
  
}

function is_empty(s) {
  return s.c === /* [] */0;
}

function length(s) {
  return s.len;
}

function iter(f, s) {
  return List.iter(f, s.c);
}

function fold(f, acc, s) {
  return List.fold_left(f, acc, s.c);
}

function to_seq(s) {
  return List.flatten(s.c);
}

function add_seq(q, i) {
  return Seq.iter((function (x) {
                return push(x, q);
              }), i);
}

function of_seq(g) {
  var s = {
    c: /* [] */0,
    len: 0
  };
  add_seq(s, g);
  return s;
}

export {
  Empty ,
  create ,
  clear ,
  copy ,
  push ,
  pop ,
  pop_opt ,
  top ,
  top_opt ,
  is_empty ,
  length ,
  iter ,
  fold ,
  to_seq ,
  add_seq ,
  of_seq ,
  
}
/* No side effect */
