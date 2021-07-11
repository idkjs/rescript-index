// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core from "./core.js";
import * as List from "@rescript/std/lib/es6/list.js";
import * as Pervasives from "@rescript/std/lib/es6/pervasives.js";
import * as Caml_string from "@rescript/std/lib/es6/caml_string.js";
import * as Caml_exceptions from "@rescript/std/lib/es6/caml_exceptions.js";
import * as Caml_js_exceptions from "@rescript/std/lib/es6/caml_js_exceptions.js";

var Parse_error = /* @__PURE__ */Caml_exceptions.create("Perl.Parse_error");

var Not_supported = /* @__PURE__ */Caml_exceptions.create("Perl.Not_supported");

function posix_class_of_string(class_) {
  switch (class_) {
    case "alnum" :
        return Core.alnum;
    case "alpha" :
        return Core.alpha;
    case "ascii" :
        return Core.ascii;
    case "blank" :
        return Core.blank;
    case "cntrl" :
        return Core.cntrl;
    case "digit" :
        return Core.digit;
    case "graph" :
        return Core.graph;
    case "lower" :
        return Core.lower;
    case "print" :
        return Core.print;
    case "punct" :
        return Core.punct;
    case "space" :
        return Core.space;
    case "upper" :
        return Core.upper;
    case "word" :
        return Core.wordc;
    case "xdigit" :
        return Core.xdigit;
    default:
      return Pervasives.invalid_arg("Invalid pcre class: " + class_);
  }
}

function parse(multiline, dollar_endonly, dotall, ungreedy, s) {
  var i = {
    contents: 0
  };
  var l = s.length;
  var test = function (c) {
    if (i.contents !== l) {
      return Caml_string.get(s, i.contents) === c;
    } else {
      return false;
    }
  };
  var accept = function (c) {
    var r = test(c);
    if (r) {
      i.contents = i.contents + 1 | 0;
    }
    return r;
  };
  var accept_s = function (s$p) {
    var len = s$p.length;
    try {
      for(var j = 0; j < len; ++j){
        try {
          if (Caml_string.get(s$p, j) !== Caml_string.get(s, i.contents + j | 0)) {
            throw {
                  RE_EXN_ID: Pervasives.Exit,
                  Error: new Error()
                };
          }
          
        }
        catch (exn){
          throw {
                RE_EXN_ID: Pervasives.Exit,
                Error: new Error()
              };
        }
      }
      i.contents = i.contents + len | 0;
      return true;
    }
    catch (raw_exn){
      var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn$1.RE_EXN_ID === Pervasives.Exit) {
        return false;
      }
      throw exn$1;
    }
  };
  var get = function (param) {
    var r = Caml_string.get(s, i.contents);
    i.contents = i.contents + 1 | 0;
    return r;
  };
  var greedy_mod = function (r) {
    var gr = accept(/* '?' */63);
    var gr$1 = ungreedy ? !gr : gr;
    if (gr$1) {
      return Core.non_greedy(r);
    } else {
      return Core.greedy(r);
    }
  };
  var $$char = function (param) {
    if (i.contents === l) {
      throw {
            RE_EXN_ID: Parse_error,
            Error: new Error()
          };
    }
    var c = get(undefined);
    if (c === /* '[' */91) {
      if (accept(/* '=' */61)) {
        throw {
              RE_EXN_ID: Not_supported,
              Error: new Error()
            };
      }
      if (accept(/* ':' */58)) {
        var compl = accept(/* '^' */94);
        var cls;
        try {
          cls = List.find(accept_s, {
                hd: "alpha",
                tl: {
                  hd: "alnum",
                  tl: {
                    hd: "ascii",
                    tl: {
                      hd: "blank",
                      tl: {
                        hd: "cntrl",
                        tl: {
                          hd: "digit",
                          tl: {
                            hd: "lower",
                            tl: {
                              hd: "print",
                              tl: {
                                hd: "space",
                                tl: {
                                  hd: "upper",
                                  tl: {
                                    hd: "word",
                                    tl: {
                                      hd: "punct",
                                      tl: {
                                        hd: "graph",
                                        tl: {
                                          hd: "xdigit",
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              });
        }
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn.RE_EXN_ID === "Not_found") {
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
          }
          throw exn;
        }
        if (!accept_s(":]")) {
          throw {
                RE_EXN_ID: Parse_error,
                Error: new Error()
              };
        }
        var posix_class = posix_class_of_string(cls);
        var re = compl ? Core.compl({
                hd: posix_class,
                tl: /* [] */0
              }) : posix_class;
        return {
                NAME: "Set",
                VAL: re
              };
      }
      if (!accept(/* '.' */46)) {
        return {
                NAME: "Char",
                VAL: c
              };
      }
      if (i.contents === l) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      var c$1 = get(undefined);
      if (!accept(/* '.' */46)) {
        throw {
              RE_EXN_ID: Not_supported,
              Error: new Error()
            };
      }
      if (!accept(/* ']' */93)) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      return {
              NAME: "Char",
              VAL: c$1
            };
    }
    if (c !== /* '\\' */92) {
      return {
              NAME: "Char",
              VAL: c
            };
    }
    if (i.contents === l) {
      throw {
            RE_EXN_ID: Parse_error,
            Error: new Error()
          };
    }
    var c$2 = get(undefined);
    if (c$2 >= 58) {
      if (c$2 >= 123) {
        return {
                NAME: "Char",
                VAL: c$2
              };
      }
      switch (c$2) {
        case 68 :
            return {
                    NAME: "Set",
                    VAL: Core.compl({
                          hd: Core.digit,
                          tl: /* [] */0
                        })
                  };
        case 83 :
            return {
                    NAME: "Set",
                    VAL: Core.compl({
                          hd: Core.space,
                          tl: /* [] */0
                        })
                  };
        case 87 :
            return {
                    NAME: "Set",
                    VAL: Core.compl({
                          hd: Core.alnum,
                          tl: {
                            hd: Core.$$char(/* '_' */95),
                            tl: /* [] */0
                          }
                        })
                  };
        case 58 :
        case 59 :
        case 60 :
        case 61 :
        case 62 :
        case 63 :
        case 64 :
        case 91 :
        case 92 :
        case 93 :
        case 94 :
        case 95 :
        case 96 :
            return {
                    NAME: "Char",
                    VAL: c$2
                  };
        case 98 :
            return {
                    NAME: "Char",
                    VAL: /* '\b' */8
                  };
        case 100 :
            return {
                    NAME: "Set",
                    VAL: Core.digit
                  };
        case 110 :
            return {
                    NAME: "Char",
                    VAL: /* '\n' */10
                  };
        case 114 :
            return {
                    NAME: "Char",
                    VAL: /* '\r' */13
                  };
        case 115 :
            return {
                    NAME: "Set",
                    VAL: Core.space
                  };
        case 116 :
            return {
                    NAME: "Char",
                    VAL: /* '\t' */9
                  };
        case 119 :
            return {
                    NAME: "Set",
                    VAL: Core.alt({
                          hd: Core.alnum,
                          tl: {
                            hd: Core.$$char(/* '_' */95),
                            tl: /* [] */0
                          }
                        })
                  };
        case 65 :
        case 66 :
        case 67 :
        case 69 :
        case 70 :
        case 71 :
        case 72 :
        case 73 :
        case 74 :
        case 75 :
        case 76 :
        case 77 :
        case 78 :
        case 79 :
        case 80 :
        case 81 :
        case 82 :
        case 84 :
        case 85 :
        case 86 :
        case 88 :
        case 89 :
        case 90 :
        case 97 :
        case 99 :
        case 101 :
        case 102 :
        case 103 :
        case 104 :
        case 105 :
        case 106 :
        case 107 :
        case 108 :
        case 109 :
        case 111 :
        case 112 :
        case 113 :
        case 117 :
        case 118 :
        case 120 :
        case 121 :
        case 122 :
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
        
      }
    } else {
      if (c$2 >= 48) {
        throw {
              RE_EXN_ID: Not_supported,
              Error: new Error()
            };
      }
      return {
              NAME: "Char",
              VAL: c$2
            };
    }
  };
  var bracket = function (_s) {
    while(true) {
      var s = _s;
      if (s !== /* [] */0 && accept(/* ']' */93)) {
        return s;
      }
      var match = $$char(undefined);
      if (match.NAME === "Char") {
        var c = match.VAL;
        if (accept(/* '-' */45)) {
          if (accept(/* ']' */93)) {
            return {
                    hd: Core.$$char(c),
                    tl: {
                      hd: Core.$$char(/* '-' */45),
                      tl: s
                    }
                  };
          }
          var match$1 = $$char(undefined);
          if (match$1.NAME === "Char") {
            _s = {
              hd: Core.rg(c, match$1.VAL),
              tl: s
            };
            continue ;
          }
          _s = {
            hd: Core.$$char(c),
            tl: {
              hd: Core.$$char(/* '-' */45),
              tl: {
                hd: match$1.VAL,
                tl: s
              }
            }
          };
          continue ;
        }
        _s = {
          hd: Core.$$char(c),
          tl: s
        };
        continue ;
      }
      _s = {
        hd: match.VAL,
        tl: s
      };
      continue ;
    };
  };
  var atom = function (param) {
    if (accept(/* '.' */46)) {
      if (dotall) {
        return Core.any;
      } else {
        return Core.notnl;
      }
    }
    if (accept(/* '(' */40)) {
      if (accept(/* '?' */63)) {
        if (accept(/* ':' */58)) {
          var r = regexp$p(branch$p(/* [] */0));
          if (!accept(/* ')' */41)) {
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
          }
          return r;
        }
        if (accept(/* '#' */35)) {
          var _param;
          while(true) {
            if (i.contents === l) {
              throw {
                    RE_EXN_ID: Parse_error,
                    Error: new Error()
                  };
            }
            if (accept(/* ')' */41)) {
              return Core.epsilon;
            }
            i.contents = i.contents + 1 | 0;
            _param = undefined;
            continue ;
          };
        }
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      var r$1 = regexp$p(branch$p(/* [] */0));
      if (!accept(/* ')' */41)) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      return Core.group(r$1);
    }
    if (accept(/* '^' */94)) {
      if (multiline) {
        return Core.bol;
      } else {
        return Core.bos;
      }
    }
    if (accept(/* '$' */36)) {
      if (multiline) {
        return Core.eol;
      } else if (dollar_endonly) {
        return Core.leol;
      } else {
        return Core.eos;
      }
    }
    if (accept(/* '[' */91)) {
      if (accept(/* '^' */94)) {
        return Core.compl(bracket(/* [] */0));
      } else {
        return Core.alt(bracket(/* [] */0));
      }
    }
    if (accept(/* '\\' */92)) {
      if (i.contents === l) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      var c = get(undefined);
      switch (c) {
        case 48 :
        case 49 :
        case 50 :
        case 51 :
        case 52 :
        case 53 :
        case 54 :
        case 55 :
        case 56 :
        case 57 :
            throw {
                  RE_EXN_ID: Not_supported,
                  Error: new Error()
                };
        case 65 :
            return Core.bos;
        case 66 :
            return Core.not_boundary;
        case 68 :
            return Core.compl({
                        hd: Core.digit,
                        tl: /* [] */0
                      });
        case 71 :
            return Core.start;
        case 83 :
            return Core.compl({
                        hd: Core.space,
                        tl: /* [] */0
                      });
        case 87 :
            return Core.compl({
                        hd: Core.alnum,
                        tl: {
                          hd: Core.$$char(/* '_' */95),
                          tl: /* [] */0
                        }
                      });
        case 90 :
            return Core.leol;
        case 58 :
        case 59 :
        case 60 :
        case 61 :
        case 62 :
        case 63 :
        case 64 :
        case 91 :
        case 92 :
        case 93 :
        case 94 :
        case 95 :
        case 96 :
            return Core.$$char(c);
        case 98 :
            return Core.alt({
                        hd: Core.bow,
                        tl: {
                          hd: Core.eow,
                          tl: /* [] */0
                        }
                      });
        case 100 :
            return Core.digit;
        case 115 :
            return Core.space;
        case 119 :
            return Core.alt({
                        hd: Core.alnum,
                        tl: {
                          hd: Core.$$char(/* '_' */95),
                          tl: /* [] */0
                        }
                      });
        case 67 :
        case 69 :
        case 70 :
        case 72 :
        case 73 :
        case 74 :
        case 75 :
        case 76 :
        case 77 :
        case 78 :
        case 79 :
        case 80 :
        case 81 :
        case 82 :
        case 84 :
        case 85 :
        case 86 :
        case 88 :
        case 89 :
        case 97 :
        case 99 :
        case 101 :
        case 102 :
        case 103 :
        case 104 :
        case 105 :
        case 106 :
        case 107 :
        case 108 :
        case 109 :
        case 110 :
        case 111 :
        case 112 :
        case 113 :
        case 114 :
        case 116 :
        case 117 :
        case 118 :
        case 120 :
        case 121 :
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
        case 122 :
            return Core.eos;
        default:
          return Core.$$char(c);
      }
    } else {
      if (i.contents === l) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      var c$1 = get(undefined);
      if (c$1 >= 64) {
        if (c$1 !== 92) {
          if (c$1 !== 123) {
            return Core.$$char(c$1);
          }
          throw {
                RE_EXN_ID: Parse_error,
                Error: new Error()
              };
        }
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      if (c$1 >= 44) {
        if (c$1 >= 63) {
          throw {
                RE_EXN_ID: Parse_error,
                Error: new Error()
              };
        }
        return Core.$$char(c$1);
      }
      if (c$1 >= 42) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      return Core.$$char(c$1);
    }
  };
  var integer = function (param) {
    if (i.contents === l) {
      return ;
    }
    var d = get(undefined);
    if (d > 57 || d < 48) {
      i.contents = i.contents - 1 | 0;
      return ;
    } else {
      var _i = d - /* '0' */48 | 0;
      while(true) {
        var i$1 = _i;
        if (i.contents === l) {
          return i$1;
        }
        var d$1 = get(undefined);
        if (d$1 > 57 || d$1 < 48) {
          i.contents = i.contents - 1 | 0;
          return i$1;
        }
        var i$p = Math.imul(10, i$1) + (d$1 - /* '0' */48 | 0) | 0;
        if (i$p < i$1) {
          throw {
                RE_EXN_ID: Parse_error,
                Error: new Error()
              };
        }
        _i = i$p;
        continue ;
      };
    }
  };
  var branch$p = function (_left) {
    while(true) {
      var left = _left;
      if (i.contents === l || test(/* '|' */124) || test(/* ')' */41)) {
        return Core.seq(List.rev(left));
      }
      _left = {
        hd: piece(undefined),
        tl: left
      };
      continue ;
    };
  };
  var piece = function (param) {
    var r = atom(undefined);
    if (accept(/* '*' */42)) {
      return greedy_mod(Core.rep(r));
    }
    if (accept(/* '+' */43)) {
      return greedy_mod(Core.rep1(r));
    }
    if (accept(/* '?' */63)) {
      return greedy_mod(Core.opt(r));
    }
    if (!accept(/* '{' */123)) {
      return r;
    }
    var i$1 = integer(undefined);
    if (i$1 !== undefined) {
      var j = accept(/* ',' */44) ? integer(undefined) : i$1;
      if (!accept(/* '}' */125)) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      if (j !== undefined && j < i$1) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      return greedy_mod(Core.repn(r, i$1, j));
    }
    i.contents = i.contents - 1 | 0;
    return r;
  };
  var regexp$p = function (_left) {
    while(true) {
      var left = _left;
      if (!accept(/* '|' */124)) {
        return left;
      }
      _left = Core.alt({
            hd: left,
            tl: {
              hd: branch$p(/* [] */0),
              tl: /* [] */0
            }
          });
      continue ;
    };
  };
  var res = regexp$p(branch$p(/* [] */0));
  if (i.contents !== l) {
    throw {
          RE_EXN_ID: Parse_error,
          Error: new Error()
        };
  }
  return res;
}

function re(optsOpt, s) {
  var opts = optsOpt !== undefined ? optsOpt : /* [] */0;
  var r = parse(List.memq("Multiline", opts), List.memq("Dollar_endonly", opts), List.memq("Dotall", opts), List.memq("Ungreedy", opts), s);
  var r$1 = List.memq("Anchored", opts) ? Core.seq({
          hd: Core.start,
          tl: {
            hd: r,
            tl: /* [] */0
          }
        }) : r;
  if (List.memq("Caseless", opts)) {
    return Core.no_case(r$1);
  } else {
    return r$1;
  }
}

function compile_pat(optsOpt, s) {
  var opts = optsOpt !== undefined ? optsOpt : /* [] */0;
  return Core.compile(re(opts, s));
}

var compile = Core.compile;

export {
  Parse_error ,
  Not_supported ,
  re ,
  compile ,
  compile_pat ,
  
}
/* Core Not a pure module */
