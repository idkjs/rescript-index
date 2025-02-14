// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Char from "@rescript/std/lib/es6/char.js";
import * as Cset from "./cset.js";
import * as Bytes from "@rescript/std/lib/es6/bytes.js";
import * as Caml_bytes from "@rescript/std/lib/es6/caml_bytes.js";

function make(param) {
  return Bytes.make(257, /* '\000' */0);
}

function flatten(cm) {
  var c = Caml_bytes.caml_create_bytes(256);
  var color_repr = Caml_bytes.caml_create_bytes(256);
  var v = 0;
  Caml_bytes.set(c, 0, /* '\000' */0);
  Caml_bytes.set(color_repr, 0, /* '\000' */0);
  for(var i = 1; i <= 255; ++i){
    if (Caml_bytes.get(cm, i) !== /* '\000' */0) {
      v = v + 1 | 0;
    }
    Caml_bytes.set(c, i, Char.chr(v));
    Caml_bytes.set(color_repr, v, Char.chr(i));
  }
  return [
          c,
          Bytes.sub(color_repr, 0, v + 1 | 0),
          v + 1 | 0
        ];
}

function split(s, cm) {
  return Cset.iter(s, (function (i, j) {
                Caml_bytes.set(cm, i, /* '\001' */1);
                return Caml_bytes.set(cm, j + 1 | 0, /* '\001' */1);
              }));
}

export {
  make ,
  flatten ,
  split ,
  
}
/* Cset Not a pure module */
