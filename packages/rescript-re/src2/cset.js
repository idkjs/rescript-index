// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fmt from "./fmt.js";
import * as $$Map from "@rescript/std/lib/es6/map.js";
import * as List from "@rescript/std/lib/es6/list.js";
import * as Curry from "@rescript/std/lib/es6/curry.js";
import * as Caml_obj from "@rescript/std/lib/es6/caml_obj.js";
import * as Pervasives from "@rescript/std/lib/es6/pervasives.js";
import * as Caml_option from "@rescript/std/lib/es6/caml_option.js";

function union(_l, _l$p) {
  while(true) {
    var l$p = _l$p;
    var l = _l;
    if (!l$p) {
      return l;
    }
    if (!l) {
      return l$p;
    }
    var r$p = l$p.tl;
    var match = l$p.hd;
    var c2$p = match[1];
    var c1$p = match[0];
    var r = l.tl;
    var match$1 = l.hd;
    var c2 = match$1[1];
    var c1 = match$1[0];
    if ((c2 + 1 | 0) < c1$p) {
      return {
              hd: [
                c1,
                c2
              ],
              tl: union(r, l$p)
            };
    }
    if ((c2$p + 1 | 0) < c1) {
      return {
              hd: [
                c1$p,
                c2$p
              ],
              tl: union(l, r$p)
            };
    }
    if (c2 < c2$p) {
      _l$p = {
        hd: [
          c1 < c1$p ? c1 : c1$p,
          c2$p
        ],
        tl: r$p
      };
      _l = r;
      continue ;
    }
    _l$p = r$p;
    _l = {
      hd: [
        c1 < c1$p ? c1 : c1$p,
        c2
      ],
      tl: r
    };
    continue ;
  };
}

function inter(_l, _l$p) {
  while(true) {
    var l$p = _l$p;
    var l = _l;
    if (!l$p) {
      return /* [] */0;
    }
    if (!l) {
      return /* [] */0;
    }
    var r$p = l$p.tl;
    var match = l$p.hd;
    var c2$p = match[1];
    var c1$p = match[0];
    var r = l.tl;
    var match$1 = l.hd;
    var c2 = match$1[1];
    var c1 = match$1[0];
    if (Caml_obj.caml_lessthan(c2, c1$p)) {
      _l = r;
      continue ;
    }
    if (!Caml_obj.caml_lessthan(c2$p, c1)) {
      if (Caml_obj.caml_lessthan(c2, c2$p)) {
        return {
                hd: [
                  Caml_obj.caml_max(c1, c1$p),
                  c2
                ],
                tl: inter(r, l$p)
              };
      } else {
        return {
                hd: [
                  Caml_obj.caml_max(c1, c1$p),
                  c2$p
                ],
                tl: inter(l, r$p)
              };
      }
    }
    _l$p = r$p;
    continue ;
  };
}

function diff(_l, _l$p) {
  while(true) {
    var l$p = _l$p;
    var l = _l;
    if (!l$p) {
      return l;
    }
    if (!l) {
      return /* [] */0;
    }
    var r$p = l$p.tl;
    var match = l$p.hd;
    var c2$p = match[1];
    var c1$p = match[0];
    var r = l.tl;
    var match$1 = l.hd;
    var c2 = match$1[1];
    var c1 = match$1[0];
    if (c2 < c1$p) {
      return {
              hd: [
                c1,
                c2
              ],
              tl: diff(r, l$p)
            };
    }
    if (c2$p < c1) {
      _l$p = r$p;
      continue ;
    }
    var r$p$p = c2$p < c2 ? ({
          hd: [
            c2$p + 1 | 0,
            c2
          ],
          tl: r
        }) : r;
    if (c1 < c1$p) {
      return {
              hd: [
                c1,
                c1$p - 1 | 0
              ],
              tl: diff(r$p$p, r$p)
            };
    }
    _l$p = r$p;
    _l = r$p$p;
    continue ;
  };
}

function single(c) {
  return {
          hd: [
            c,
            c
          ],
          tl: /* [] */0
        };
}

function add(c, l) {
  return union(single(c), l);
}

function seq(c, c$p) {
  if (Caml_obj.caml_lessequal(c, c$p)) {
    return {
            hd: [
              c,
              c$p
            ],
            tl: /* [] */0
          };
  } else {
    return {
            hd: [
              c$p,
              c
            ],
            tl: /* [] */0
          };
  }
}

function offset(o, l) {
  if (!l) {
    return /* [] */0;
  }
  var match = l.hd;
  return {
          hd: [
            match[0] + o | 0,
            match[1] + o | 0
          ],
          tl: offset(o, l.tl)
        };
}

function mem(c, _s) {
  while(true) {
    var s = _s;
    if (!s) {
      return false;
    }
    var match = s.hd;
    if (c <= match[1]) {
      return c >= match[0];
    }
    _s = s.tl;
    continue ;
  };
}

function hash_rec(param) {
  if (!param) {
    return 0;
  }
  var match = param.hd;
  return (match[0] + Math.imul(13, match[1]) | 0) + Math.imul(257, hash_rec(param.tl)) | 0;
}

function hash(l) {
  return hash_rec(l) & 1073741823;
}

function print_one(ch, param) {
  var c2 = param[1];
  var c1 = param[0];
  if (Caml_obj.caml_equal(c1, c2)) {
    console.log(ch, "%d", c1);
  } else {
    console.log(ch, "%d-%d", c1, c2);
  }
  
}

var partial_arg = (function (prim) {
    console.log(prim);
    
  });

function pp(param, param$1) {
  return Fmt.list(partial_arg, print_one, param, param$1);
}

function iter(_t, f) {
  while(true) {
    var t = _t;
    if (!t) {
      return ;
    }
    var match = t.hd;
    Curry._2(f, match[0], match[1]);
    _t = t.tl;
    continue ;
  };
}

function one_char(param) {
  if (!param) {
    return ;
  }
  if (param.tl) {
    return ;
  }
  var match = param.hd;
  var i = match[0];
  if (Caml_obj.caml_equal(i, match[1])) {
    return Caml_option.some(i);
  }
  
}

function compare(param, param$1) {
  var c = Caml_obj.caml_compare(param[0], param$1[0]);
  if (c !== 0) {
    return c;
  } else {
    return Caml_obj.caml_compare(param[1], param$1[1]);
  }
}

var CSetMap = $$Map.Make({
      compare: compare
    });

function fold_right(t, init, f) {
  return List.fold_right(f, t, init);
}

var csingle = single;

function is_empty(param) {
  if (param) {
    return false;
  } else {
    return true;
  }
}

function prepend(_s, x, l) {
  while(true) {
    var s = _s;
    if (!s) {
      return l;
    }
    if (!l) {
      return /* [] */0;
    }
    var match = l.hd;
    var match$1 = match[0];
    if (match$1) {
      if (match$1.tl) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "cset.re",
                194,
                9
              ],
              Error: new Error()
            };
      }
      var _r$p = l.tl;
      var _x$p = match[1];
      var match$2 = match$1.hd;
      var _d$p = match$2[1];
      var d = match$2[0];
      var r = s.tl;
      var match$3 = s.hd;
      var c$p = match$3[1];
      var _c = match$3[0];
      if (c$p >= d) {
        if (_c <= d) {
          if (c$p < _d$p) {
            return {
                    hd: [
                      {
                        hd: [
                          d,
                          c$p
                        ],
                        tl: /* [] */0
                      },
                      Pervasives.$at(x, _x$p)
                    ],
                    tl: prepend(r, x, {
                          hd: [
                            {
                              hd: [
                                c$p + 1 | 0,
                                _d$p
                              ],
                              tl: /* [] */0
                            },
                            _x$p
                          ],
                          tl: _r$p
                        })
                  };
          } else {
            return {
                    hd: [
                      {
                        hd: [
                          d,
                          _d$p
                        ],
                        tl: /* [] */0
                      },
                      Pervasives.$at(x, _x$p)
                    ],
                    tl: prepend(s, x, _r$p)
                  };
          }
        } else if (_c > _d$p) {
          return {
                  hd: [
                    {
                      hd: [
                        d,
                        _d$p
                      ],
                      tl: /* [] */0
                    },
                    _x$p
                  ],
                  tl: prepend(s, x, _r$p)
                };
        } else {
          return {
                  hd: [
                    {
                      hd: [
                        d,
                        _c - 1 | 0
                      ],
                      tl: /* [] */0
                    },
                    _x$p
                  ],
                  tl: prepend(s, x, {
                        hd: [
                          {
                            hd: [
                              _c,
                              _d$p
                            ],
                            tl: /* [] */0
                          },
                          _x$p
                        ],
                        tl: _r$p
                      })
                };
        }
      }
      _s = r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "cset.re",
            194,
            9
          ],
          Error: new Error()
        };
  };
}

function pick(param) {
  if (param) {
    return param.hd[0];
  } else {
    return Pervasives.invalid_arg("Re_cset.pick");
  }
}

var empty = /* [] */0;

var cany = {
  hd: [
    0,
    255
  ],
  tl: /* [] */0
};

export {
  iter ,
  union ,
  inter ,
  diff ,
  offset ,
  empty ,
  single ,
  seq ,
  add ,
  mem ,
  hash ,
  pp ,
  one_char ,
  fold_right ,
  hash_rec ,
  CSetMap ,
  cany ,
  csingle ,
  is_empty ,
  prepend ,
  pick ,
  
}
/* CSetMap Not a pure module */
