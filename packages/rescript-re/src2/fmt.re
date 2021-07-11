/** Very small tooling for format printers. */;

// include Format;

type t('a) = ( 'a) => unit;

/* Only in the stdlib since 4.02, so we copy. */
let rec list = (~pp_sep=Js.log, pp, ppf) =>
  fun
  | [] => ()
  | [v] => pp(ppf, v)
  | [v, ...vs] => {
      pp(ppf, v);
      pp_sep(ppf);
      list(~pp_sep, pp, ppf, vs);
    };

/* want this name to make sure we don't use Js.log2 from stdlib
   accidentally */
// let pp_print_list = list;

// let str = Js.String.t;
let sexp = (fmt, s, pp, x) => Js.logMany([|fmt, "@[<3>(%s@ %a)@]", s, pp, x|]);
let pair = (v1, v2) => {
  Js.log( v1);

  Js.log( v2);
};
// let pair = (pp1, pp2, fmt, (v1, v2)) => {
//   pp1(fmt, v1);
//   pp_print_space(fmt, ());
//   pp2(fmt, v2);
// };
let triple =Js.log
// let triple = (pp1, pp2, pp3, fmt, (v1, v2, v3)) => {
//   pp1(fmt, v1);
//   pp_print_space(fmt, ());
//   pp2(fmt, v2);
//   pp_print_space(fmt, ());
//   pp3(fmt, v3);
// };
// let int = pp_print_int;
let optint = fmt =>
  fun
  | None => ()
  | Some(i) => Js.log3(fmt, "@ %d", i);

let quote = (fmt, s) => Js.log3(fmt, "\"%s\"", s);

let pp_olist = (pp_elem, fmt) =>
  Js.log4(
    "@[<3>[@ %a@ ]@]" ++
    ";@ " ++ pp_elem
  );

// let pp_str_list = pp_olist(quote);

let to_to_string = (pp, x) => {
  let b = Buffer.create(16);
  let fmt = Js.log(b);
  pp(fmt, x);
  Buffer.contents(b);
};
