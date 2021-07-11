/* Result of a successful match. */
type t = {
  s: string,
  marks: Automata.mark_infos,
  pmarks: Pmark.Set.t,
  gpos: array(int),
  gcount: int,
};

let offset = (t, i) => {
  if (2 * i + 1 >= Array.length(t.marks)) {
    raise(Not_found);
  };
  let m1 = t.marks[2 * i];
  if (m1 == (-1)) {
    raise(Not_found);
  };
  let p1 = t.gpos[m1] - 1;
  let p2 = t.gpos[t.marks[2 * i + 1]] - 1;
  (p1, p2);
};

let get = (t, i) => {
  let (p1, p2) = offset(t, i);
  String.sub(t.s, p1, p2 - p1);
};

let start = (subs, i) => fst(offset(subs, i));

let stop = (subs, i) => snd(offset(subs, i));

let test = (t, i) =>
  if (2 * i >= Array.length(t.marks)) {
    false;
  } else {
    let idx = t.marks[2 * i];
    idx != (-1);
  };

let get_opt = (t, i) =>
  if (test(t, i)) {
    Some(get(t, i));
  } else {
    None;
  };

let dummy_offset = ((-1), (-1));

let all_offset = t => {
  let res = Array.make(t.gcount, dummy_offset);
  for (i in 0 to Array.length(t.marks) / 2 - 1) {
    let m1 = t.marks[2 * i];
    if (m1 != (-1)) {
      let p1 = t.gpos[m1];
      let p2 = t.gpos[t.marks[2 * i + 1]];
      res[i] = (p1 - 1, p2 - 1);
    };
  };
  res;
};

let dummy_string = "";

let all = t => {
  let res = Array.make(t.gcount, dummy_string);
  for (i in 0 to Array.length(t.marks) / 2 - 1) {
    let m1 = t.marks[2 * i];
    if (m1 != (-1)) {
      let p1 = t.gpos[m1];
      let p2 = t.gpos[t.marks[2 * i + 1]];
      res[i] = String.sub(t.s, p1 - 1, p2 - p1);
    };
  };
  res;
};

// let pp = (fmt, t) => {
//   let matches = {
//     let offsets = all_offset(t);
//     let strs = all(t);
//     Array.to_list(
//       Array.init(Array.length(strs), i => (strs[i], offsets[i])),
//     );
//   };
//   // open Fmt;
//   let pp_match = (fmt, (str, (start, stop))) =>
//     Js.logMany([|fmt, "@[(%s (%d %d))@]", str, start, stop|]);
//   Js.log4(fmt, "Group", Fmt.list(pp_match), matches);
// };
let pp = (fmt, t) => {
  let matches = {
    let offsets = all_offset(t);
    let strs = all(t);
    Array.to_list(
      Array.init(Array.length(strs), i => (strs[i], offsets[i])),
    );
  };
  open Fmt;
  let pp_match = (fmt, (str, (start, stop))) =>
    Js.logMany([|fmt, "@[(%s (%d %d))@]", str, start, stop|]);
  sexp(fmt, "Group", pp_match->Obj.magic, matches->Obj.magic);
};
let nb_groups = t => t.gcount;
