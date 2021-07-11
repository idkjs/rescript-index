/** Very small tooling for format printers. */;

include Format;

type t('a) = (Format.formatter, 'a) => unit;

/* Only in the stdlib since 4.02, so we copy. */
let rec list = (~pp_sep=pp_print_cut, pp, ppf) =>
  fun
  | [] => ()
  | [v] => pp(ppf, v)
  | [v, ...vs] => {
      pp(ppf, v);
      pp_sep(ppf, ());
      list(~pp_sep, pp, ppf, vs);
    };

/* want this name to make sure we don't use pp_print_list from stdlib
   accidentally */
let pp_print_list = list;

let str = pp_print_string;
let sexp = (fmt, s, pp, x) => fprintf(fmt, Obj.magic("@[<3>(%s@ %a)@]"), s, pp, x,);
let pair = (pp1, pp2, fmt, (v1, v2)) => {
  pp1(fmt, v1);
  pp_print_space(fmt, ());
  pp2(fmt, v2);
};
let triple = (pp1, pp2, pp3, fmt, (v1, v2, v3)) => {
  pp1(fmt, v1);
  pp_print_space(fmt, ());
  pp2(fmt, v2);
  pp_print_space(fmt, ());
  pp3(fmt, v3);
};
let int = pp_print_int;
let optint = fmt =>
  fun
  | None => ()
  | Some(i) => Printf.sprintf(fmt, Obj.magic("@ %d"), i);

// let quote = (fmt, s) => Format.fprintf(fmt, Obj.magic("\"%s\""), s)->ignore;
let quote = Printf.sprintf("'%s'");
// let pp_olist = (pp_elem, fmt) =>
//   Format.fprintf(
//     fmt,
//     Obj.magic("@[<3>[@ %a@ ]@]"),
//     pp_print_list(~pp_sep=(fmt, ()) => fprintf(fmt, Obj.magic(";@ ")), pp_elem)->ignore
//   );

// let pp_str_list = pp_olist(quote);
// let pp_str_list = pp_olist(quote);

let to_to_string = (pp, x) => {
  let b = Buffer.create(16);
  let fmt = Format.formatter_of_buffer(b);
  pp(fmt, x);
  Buffer.contents(b);
};
