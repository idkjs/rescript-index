/*
    RE - A regular expression library

    Copyright (C) 2001 Jerome Vouillon
    email: Jerome.Vouillon@pps.jussieu.fr

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation, with
    linking exception; either version 2.1 of the License, or (at
    your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

type sem = [ | `Longest | `Shortest | `First];

type rep_kind = [ | `Greedy | `Non_greedy];

type mark = int;
type idx = int;

type expr = {
  id: int,
  def,
}

and def =
  | Cst(Cset.t)
  | Alt(list(expr))
  | Seq(sem, expr, expr)
  | Eps
  | Rep(rep_kind, sem, expr)
  | Mark(int)
  | Erase(int, int)
  | Before(Category.t)
  | After(Category.t)
  | Pmark(Pmark.t);

let hash_combine = (h, accu) => accu * 65599 + h;

module Marks = {
  type t = {
    marks: list((int, int)),
    pmarks: Pmark.Set.t,
  };

  let empty = {marks: [], pmarks: Pmark.Set.empty};

  let rec merge_marks_offset = old =>
    fun
    | [] => old
    | [(i, v), ...rem] => {
        let nw' = merge_marks_offset(List.remove_assq(i, old), rem);
        if (v == (-2)) {
          nw';
        } else {
          [(i, v), ...nw'];
        };
      };

  let merge = (old, nw) => {
    marks: merge_marks_offset(old.marks, nw.marks),
    pmarks: Pmark.Set.union(old.pmarks, nw.pmarks),
  };

  let rec hash_marks_offset = (l, accu) =>
    switch (l) {
    | [] => accu
    | [(a, i), ...r] =>
      hash_marks_offset(r, hash_combine(a, hash_combine(i, accu)))
    };

  let hash = (m, accu) =>
    hash_marks_offset(m.marks, hash_combine(Hashtbl.hash(m.pmarks), accu));

  let rec marks_set_idx = idx =>
    fun
    | [(a, (-1)), ...rem] => [(a, idx), ...marks_set_idx(idx, rem)]
    | marks => marks;

  let marks_set_idx = (marks, idx) => {
    ...marks,
    marks: marks_set_idx(idx, marks.marks),
  };

  let pp_marks = (ch, t) =>
    switch (t.marks) {
    | [] => ()
    | [(a, i), ...r] =>
      Js.log4(ch, "%d-%d", a, i);
      List.iter(((a, i)) => Js.log4(ch, " %d-%d", a, i), r);
    };
};

/****/

let pp_sem = (ch, k) =>
  Format.pp_print_string(
    ch,
    switch (k) {
    | `Shortest => "short"
    | `Longest => "long"
    | `First => "first"
    },
  );

let pp_rep_kind = fmt =>
  fun
  | `Greedy => Format.pp_print_string(fmt, "Greedy")
  | `Non_greedy => Format.pp_print_string(fmt, "Non_greedy");
 let print_one = (ch, (c1, c2)) =>
  if (c1 == c2) {
    Js.log3(ch, "%d", c1);
  } else {
    Js.log4(ch, "%d-%d", c1, c2);
  };
let rec pp = (ch, e) =>
  switch (e.def) {
  | Cst(l) => Js.log4(ch, "cst", Cset.pp, l)
  | Alt(l) => Js.log4(ch, "alt", Js.log(pp), l)
  | Seq(k, e, e') =>
    Js.log4(ch, "seq", Js.log4(pp_sem, pp, pp), (k, e, e'))
  | Eps => Js.log2(ch, "eps")
  | Rep(_rk, k, e) => Js.log4(ch, "rep", Js.log2(pp_sem, pp), (k, e))
  | Mark(i) => Js.log3(ch, "mark", i)
  | Pmark(i) => Js.log3(ch, "pmark", i)
  | Erase(b, e) => Js.log4(ch, "erase", b, e)
  | Before(c) => Js.log4(ch, "before", Category.pp, c)
  | After(c) => Js.log4(ch, "after", Category.pp, c)
  };

/****/

let rec first = f =>
  fun
  | [] => None
  | [x, ...r] =>
    switch (f(x)) {
    | None => first(f, r)
    | Some(_) as res => res
    };

/****/

type ids = ref(int);
let create_ids = () => ref(0);

let eps_expr = {id: 0, def: Eps};

let mk_expr = (ids, def) => {
  incr(ids);
  {id: ids^, def};
};

let empty = ids => mk_expr(ids, Alt([]));

let cst = (ids, s) =>
  if (Cset.is_empty(s)) {
    empty(ids);
  } else {
    mk_expr(ids, Cst(s));
  };

let alt = ids =>
  fun
  | [] => empty(ids)
  | [c] => c
  | l => mk_expr(ids, Alt(l));

let seq = (ids, kind, x, y) =>
  switch (x.def, y.def) {
  | (Alt([]), _) => x
  | (_, Alt([])) => y
  | (Eps, _) => y
  | (_, Eps) when kind == `First => x
  | _ => mk_expr(ids,  Seq(kind, x, y))
  };

let is_eps = expr =>
  switch (expr.def) {
  | Eps => true
  | _ => false
  };

let eps = ids => mk_expr(ids, Eps);

let rep = (ids, kind, sem, x) =>
  mk_expr(ids,  Rep(kind, sem, x));

let mark = (ids, m) => mk_expr(ids, Mark(m));

let pmark = (ids, i) => mk_expr(ids, Pmark(i));

let erase = (ids, m, m') => mk_expr(ids,  Erase(m, m'));

let before = (ids, c) => mk_expr(ids, Before(c));

let after = (ids, c) => mk_expr(ids, After(c));

/****/

let rec rename = (ids, x) =>
  switch (x.def) {
  | Cst(_)
  | Eps
  | Mark(_)
  | Pmark(_)
  | Erase(_)
  | Before(_)
  | After(_) => mk_expr(ids, x.def)
  | Alt(l) => mk_expr(ids, Alt(List.map(rename(ids), l)))
  |  Seq(k, y, z) =>
    mk_expr(ids,  Seq(k, rename(ids, y), rename(ids, z)))
  |  Rep(g, k, y) =>
    mk_expr(ids,  Rep(g, k, rename(ids, y)))
  };

/****/

type hash = int;
type mark_infos = array(int);
type status =
  | Failed
  | Match(mark_infos, Pmark.Set.t)
  | Running;

module E = {
  type t =
    | TSeq(list(t), expr, sem)
    | TExp(Marks.t, expr)
    | TMatch(Marks.t);

  let rec equal = (l1, l2) =>
    switch (l1, l2) {
    | ([], []) => true
    | (
        [ TSeq(l1', e1, _), ...r1],
        [ TSeq(l2', e2, _), ...r2],
      ) =>
      e1.id == e2.id && equal(l1', l2') && equal(r1, r2)
    | (
        [ TExp(marks1, e1), ...r1],
        [ TExp(marks2, e2), ...r2],
      ) =>
      e1.id == e2.id && marks1 == marks2 && equal(r1, r2)
    | ([TMatch(marks1), ...r1], [TMatch(marks2), ...r2]) =>
      marks1 == marks2 && equal(r1, r2)
    | _ => false
    };

  let rec hash = (l, accu) =>
    switch (l) {
    | [] => accu
    | [ TSeq(l', e, _), ...r] =>
      hash(r, hash_combine(0x172a1bce, hash_combine(e.id, hash(l', accu))))
    | [ TExp(marks, e), ...r] =>
      hash(
        r,
        hash_combine(
          0x2b4c0d77,
          hash_combine(e.id, Marks.hash(marks, accu)),
        ),
      )
    | [TMatch(marks), ...r] =>
      hash(r, hash_combine(0x1c205ad5, Marks.hash(marks, accu)))
    };

  let texp = (marks, x) =>  TExp(marks, x);

  let tseq = (kind, x, y, rem) =>
    switch (x) {
    | [] => rem
    | [ TExp(marks, {def: Eps, _})] => [
         TExp(marks, y),
        ...rem,
      ]
    | _ => [ TSeq(x, y, kind), ...rem]
    };

  let rec print_state_rec = (ch, e, y) =>
    switch (e) {
    | TMatch(marks) =>
      Js.log4(ch, "@[<2>(Match@ %a)@]", Marks.pp_marks, marks)
    | TSeq(l', x, _kind) =>
      Js.log2(ch, "@[<2>(Seq@ ");
      print_state_lst(ch, l', x)|>ignore;
      Js.log4(ch, "@ %a)@]", pp, x);
    | TExp(marks, {def: Eps, _}) =>
      Js.logMany([|
        ch,
        "@[<2>(Exp@ %d@ (%a)@ (eps))@]",
        y.id->Belt.Int.toString,
        Obj.magic(Marks.pp_marks),
        Obj.magic(marks),
      |])
    | TExp(marks, x) =>
      Js.logMany([|
        ch,
        "@[<2>(Exp@ %d@ (%a)@ %a)@]",
        Belt.Int.toString(x.id),
        Obj.magic(Marks.pp_marks),
        Obj.magic(marks),
        Obj.magic(x),
      |])
    }
 and print_state_lst = (ch, l, y) =>
    switch (l) {
    | [] => Js.log2(ch, "()")
    | [e, ...rem] =>
      print_state_rec(ch, e, y);
      List.iter(
        e => {
          Js.log2(ch, "@ | ");
          print_state_rec(ch, e, y);
        },
        rem,
      );
    };

  let pp = (ch, t) => print_state_lst(ch, [t], {id: 0, def: Eps});
};

module State = {
  type t = {
    idx,
    category: Category.t,
    desc: list(E.t),
    mutable status: option(status),
    hash,
  };

  let dummy = {
    idx: (-1),
    category: Category.dummy,
    desc: [],
    status: None,
    hash: (-1),
  };

  let hash = (idx, cat, desc) =>
    E.hash(desc, hash_combine(idx, hash_combine(Category.to_int(cat), 0)))
    land 0x3FFFFFFF;

  let mk = (idx, cat, desc) => {
    idx,
    category: cat,
    desc,
    status: None,
    hash: hash(idx, cat, desc),
  };

  let create = (cat, e) =>
    mk(0, cat, [ E.TExp(Marks.empty, e)]);

  let equal = (x, y) =>
    (x.hash: int) == y.hash
    && (x.idx: int) == y.idx
    && Category.equal(x.category, y.category)
    && E.equal(x.desc, y.desc);

  let compare = (x, y) => {
    let c = compare(x.hash: int, y.hash);
    if (c != 0) {
      c;
    } else {
      let c = Category.compare(x.category, y.category);
      if (c != 0) {
        c;
      } else {
        compare(x.desc, y.desc);
      };
    };
  };

  type t' = t;
  module Table =
    Hashtbl.Make({
      type t = t';
      let equal = equal;
      let hash = t => t.hash;
    });
};

/**** Find a free index ****/

type working_area = ref(array(bool));

let create_working_area = () => ref([|false|]);

let index_count = w => Array.length(w^);

let reset_table = a => Array.fill(a, 0, Array.length(a), false);

let rec mark_used_indices = tbl =>
  List.iter(
    fun
    |  E.TSeq(l, _, _) => mark_used_indices(tbl, l)
    |  E.TExp(marks, _)
    | E.TMatch(marks) =>
      List.iter(
        ((_, i)) =>
          if (i >= 0) {
            tbl[i] = true;
          },
        marks.Marks.marks,
      ),
  );

let rec find_free = (tbl, idx, len) =>
  if (idx == len || !tbl[idx]) {
    idx;
  } else {
    find_free(tbl, idx + 1, len);
  };

let free_index = (tbl_ref, l) => {
  let tbl = tbl_ref^;
  reset_table(tbl);
  mark_used_indices(tbl, l);
  let len = Array.length(tbl);
  let idx = find_free(tbl, 0, len);
  if (idx == len) {
    tbl_ref := Array.make(2 * len, false);
  };
  idx;
};

/**** Computation of the next state ****/

let remove_matches =
  List.filter(
    fun
    | E.TMatch(_) => false
    | _ => true,
  );

let rec split_at_match_rec = l' =>
  fun
  | [] => assert(false)
  | [E.TMatch(_), ...r] => (List.rev(l'), remove_matches(r))
  | [x, ...r] => split_at_match_rec([x, ...l'], r);

let split_at_match = l => split_at_match_rec([], l);

let rec remove_duplicates = (prev, l, y) =>
  switch (l) {
  | [] => ([], prev)
  | [E.TMatch(_) as x, ..._] =>
    /* Truncate after first match */
    ([x], prev)
  | [ E.TSeq(l', x, kind), ...r] =>
    let (l'', prev') = remove_duplicates(prev, l', x);
    let (r', prev'') = remove_duplicates(prev', r, y);
    (E.tseq(kind, l'', x, r'), prev'');
  | [ E.TExp(_marks, {def: Eps, _}) as e, ...r] =>
    if (List.memq(y.id, prev)) {
      remove_duplicates(prev, r, y);
    } else {
      let (r', prev') = remove_duplicates([y.id, ...prev], r, y);
      ([e, ...r'], prev');
    }
  | [ E.TExp(_marks, x) as e, ...r] =>
    if (List.memq(x.id, prev)) {
      remove_duplicates(prev, r, y);
    } else {
      let (r', prev') = remove_duplicates([x.id, ...prev], r, y);
      ([e, ...r'], prev');
    }
  };

let rec set_idx = idx =>
  fun
  | [] => []
  | [E.TMatch(marks), ...r] => [
      E.TMatch(Marks.marks_set_idx(marks, idx)),
      ...set_idx(idx, r),
    ]
  | [ E.TSeq(l', x, kind), ...r] => [
       E.TSeq(set_idx(idx, l'), x, kind),
      ...set_idx(idx, r),
    ]
  | [ E.TExp(marks, x), ...r] => [
       E.TExp(Marks.marks_set_idx(marks, idx), x),
      ...set_idx(idx, r),
    ];

let filter_marks = (b, e, marks) => {
  ...marks,
  Marks.marks: List.filter(((i, _)) => i < b || i > e, marks.Marks.marks),
};

let rec delta_1 = (marks, c, ~next_cat, ~prev_cat, x, rem) =>
  /*Format.eprintf "%d@." x.id;*/
  switch (x.def) {
  | Cst(s) =>
    if (Cset.mem(c, s)) {
      [E.texp(marks, eps_expr), ...rem];
    } else {
      rem;
    }
  | Alt(l) => delta_2(marks, c, ~next_cat, ~prev_cat, l, rem)
  |  Seq(kind, y, z) =>
    let y' = delta_1(marks, c, ~next_cat, ~prev_cat, y, []);
    delta_seq(c, ~next_cat, ~prev_cat, kind, y', z, rem);
  |  Rep(rep_kind, kind, y) =>
    let y' = delta_1(marks, c, ~next_cat, ~prev_cat, y, []);
    let (y'', marks') =
      switch (
        first(
          fun
          | E.TMatch(marks) => Some(marks)
          | _ => None,
          y',
        )
      ) {
      | None => (y', marks)
      | Some(marks') => (remove_matches(y'), marks')
      };

    switch (rep_kind) {
    | `Greedy => E.tseq(kind, y'', x, [E.TMatch(marks'), ...rem])
    | `Non_greedy => [E.TMatch(marks), ...E.tseq(kind, y'', x, rem)]
    };
  | Eps => [E.TMatch(marks), ...rem]
  | Mark(i) =>
    let marks = {
      ...marks,
      Marks.marks: [(i, (-1)), ...List.remove_assq(i, marks.Marks.marks)],
    };
    [E.TMatch(marks), ...rem];
  | Pmark(i) =>
    let marks = {
      ...marks,
      Marks.pmarks: Pmark.Set.add(i, marks.Marks.pmarks),
    };
    [E.TMatch(marks), ...rem];
  |  Erase(b, e) => [
      E.TMatch(filter_marks(b, e, marks)),
      ...rem,
    ]
  | Before(cat'') =>
    if (Category.intersect(next_cat, cat'')) {
      [E.TMatch(marks), ...rem];
    } else {
      rem;
    }
  | After(cat'') =>
    if (Category.intersect(prev_cat, cat'')) {
      [E.TMatch(marks), ...rem];
    } else {
      rem;
    }
  }

and delta_2 = (marks, c, ~next_cat, ~prev_cat, l, rem) =>
  switch (l) {
  | [] => rem
  | [y, ...r] =>
    delta_1(
      marks,
      c,
      ~next_cat,
      ~prev_cat,
      y,
      delta_2(marks, c, ~next_cat, ~prev_cat, r, rem),
    )
  }

and delta_seq = (c, ~next_cat, ~prev_cat, kind, y, z, rem) =>
  switch (
    first(
      fun
      | E.TMatch(marks) => Some(marks)
      | _ => None,
      y,
    )
  ) {
  | None => E.tseq(kind, y, z, rem)
  | Some(marks) =>
    switch (kind) {
    | `Longest =>
      E.tseq(
        kind,
        remove_matches(y),
        z,
        delta_1(marks, c, ~next_cat, ~prev_cat, z, rem),
      )
    | `Shortest =>
      delta_1(
        marks,
        c,
        ~next_cat,
        ~prev_cat,
        z,
        E.tseq(kind, remove_matches(y), z, rem),
      )
    | `First =>
      let (y', y'') = split_at_match(y);
      E.tseq(
        kind,
        y',
        z,
        delta_1(
          marks,
          c,
          ~next_cat,
          ~prev_cat,
          z,
          E.tseq(kind, y'', z, rem),
        ),
      );
    }
  };

let rec delta_3 = (c, ~next_cat, ~prev_cat, x, rem) =>
  switch (x) {
  |  E.TSeq(y, z, kind) =>
    let y' = delta_4(c, ~next_cat, ~prev_cat, y, []);
    delta_seq(c, ~next_cat, ~prev_cat, kind, y', z, rem);
  |  E.TExp(marks, e) =>
    delta_1(marks, c, ~next_cat, ~prev_cat, e, rem)
  | E.TMatch(_) => [x, ...rem]
  }

and delta_4 = (c, ~next_cat, ~prev_cat, l, rem) =>
  switch (l) {
  | [] => rem
  | [y, ...r] =>
    delta_3(
      c,
      ~next_cat,
      ~prev_cat,
      y,
      delta_4(c, ~next_cat, ~prev_cat, r, rem),
    )
  };

let delta = (tbl_ref, next_cat, char, st) => {
  let prev_cat = st.State.category;
  let (expr', _) =
    remove_duplicates(
      [],
      delta_4(char, ~next_cat, ~prev_cat, st.State.desc, []),
      eps_expr,
    );
  let idx = free_index(tbl_ref, expr');
  let expr'' = set_idx(idx, expr');
  State.mk(idx, next_cat, expr'');
};

/****/

let rec red_tr =
  fun
  | ([] | [_]) as l => l
  | [(s1, st1) as tr1, (s2, st2) as tr2, ...rem] =>
    if (State.equal(st1, st2)) {
      red_tr([(Cset.union(s1, s2), st1), ...rem]);
    } else {
      [tr1, ...red_tr([tr2, ...rem])];
    };

let simpl_tr = l =>
  List.sort(
    ((s1, _), (s2, _)) => compare(s1, s2),
    red_tr(
      List.sort(((_, st1), (_, st2)) => State.compare(st1, st2), l),
    ),
  );

/****/

let prepend_deriv = List.fold_right(((s, x), l) => Cset.prepend(s, x, l));

let rec restrict = s =>
  fun
  | [] => []
  | [(s', x'), ...rem] => {
      let s'' = Cset.inter(s, s');
      if (Cset.is_empty(s'')) {
        restrict(s, rem);
      } else {
        [(s'', x'), ...restrict(s, rem)];
      };
    };

let rec remove_marks = (b, e, rem) =>
  if (b > e) {
    rem;
  } else {
    remove_marks(b, e - 1, [(e, (-2)), ...rem]);
  };

let rec prepend_marks_expr = m =>
  fun
  |  E.TSeq(l, e', s) =>
     E.TSeq(prepend_marks_expr_lst(m, l), e', s)
  |  E.TExp(m', e') =>
     E.TExp(Marks.merge(m, m'), e')
  | E.TMatch(m') => E.TMatch(Marks.merge(m, m'))

and prepend_marks_expr_lst = (m, l) => List.map(prepend_marks_expr(m), l);

let prepend_marks = m =>
  List.map(((s, x)) => (s, prepend_marks_expr_lst(m, x)));

let rec deriv_1 = (all_chars, categories, marks, cat, x, rem) =>
  switch (x.def) {
  | Cst(s) => Cset.prepend(s, [E.texp(marks, eps_expr)], rem)
  | Alt(l) => deriv_2(all_chars, categories, marks, cat, l, rem)
  |  Seq(kind, y, z) =>
    let y' =
      deriv_1(all_chars, categories, marks, cat, y, [(all_chars, [])]);
    deriv_seq(all_chars, categories, cat, kind, y', z, rem);
  |  Rep(rep_kind, kind, y) =>
    let y' =
      deriv_1(all_chars, categories, marks, cat, y, [(all_chars, [])]);
    List.fold_right(
      ((s, z), rem) => {
        let (z', marks') =
          switch (
            first(
              fun
              | E.TMatch(marks) => Some(marks)
              | _ => None,
              z,
            )
          ) {
          | None => (z, marks)
          | Some(marks') => (remove_matches(z), marks')
          };

        Cset.prepend(
          s,
          switch (rep_kind) {
          | `Greedy => E.tseq(kind, z', x, [E.TMatch(marks')])
          | `Non_greedy => [E.TMatch(marks), ...E.tseq(kind, z', x, [])]
          },
          rem,
        );
      },
      y',
      rem,
    );
  | Eps => Cset.prepend(all_chars, [E.TMatch(marks)], rem)
  | Mark(i) =>
    Cset.prepend(
      all_chars,
      [
        E.TMatch({
          ...marks,
          Marks.marks: [
            (i, (-1)),
            ...List.remove_assq(i, marks.Marks.marks),
          ],
        }),
      ],
      rem,
    )
  | Pmark(_) => Cset.prepend(all_chars, [E.TMatch(marks)], rem)
  |  Erase(b, e) =>
    Cset.prepend(
      all_chars,
      [
        E.TMatch({
          ...marks,
          Marks.marks:
            remove_marks(b, e, filter_marks(b, e, marks).Marks.marks),
        }),
      ],
      rem,
    )
  | Before(cat') =>
    Cset.prepend(List.assq(cat', categories), [E.TMatch(marks)], rem)
  | After(cat') =>
    if (Category.intersect(cat, cat')) {
      Cset.prepend(all_chars, [E.TMatch(marks)], rem);
    } else {
      rem;
    }
  }

and deriv_2 = (all_chars, categories, marks, cat, l, rem) =>
  switch (l) {
  | [] => rem
  | [y, ...r] =>
    deriv_1(
      all_chars,
      categories,
      marks,
      cat,
      y,
      deriv_2(all_chars, categories, marks, cat, r, rem),
    )
  }

and deriv_seq = (all_chars, categories, cat, kind, y, z, rem) =>
  if (List.exists(
        ((_s, xl)) =>
          List.exists(
            fun
            | E.TMatch(_) => true
            | _ => false,
            xl,
          ),
        y,
      )) {
    let z' =
      deriv_1(
        all_chars,
        categories,
        Marks.empty,
        cat,
        z,
        [(all_chars, [])],
      );
    List.fold_right(
      ((s, y), rem) =>
        switch (
          first(
            fun
            | E.TMatch(marks) => Some(marks)
            | _ => None,
            y,
          )
        ) {
        | None => Cset.prepend(s, E.tseq(kind, y, z, []), rem)
        | Some(marks) =>
          let z'' = prepend_marks(marks, z');
          switch (kind) {
          | `Longest =>
            Cset.prepend(
              s,
              E.tseq(kind, remove_matches(y), z, []),
              prepend_deriv(restrict(s, z''), rem),
            )
          | `Shortest =>
            prepend_deriv(
              restrict(s, z''),
              Cset.prepend(s, E.tseq(kind, remove_matches(y), z, []), rem),
            )
          | `First =>
            let (y', y'') = split_at_match(y);
            Cset.prepend(
              s,
              E.tseq(kind, y', z, []),
              prepend_deriv(
                restrict(s, z''),
                Cset.prepend(s, E.tseq(kind, y'', z, []), rem),
              ),
            );
          };
        },
      y,
      rem,
    );
  } else {
    List.fold_right(
      ((s, xl), rem) => Cset.prepend(s, E.tseq(kind, xl, z, []), rem),
      y,
      rem,
    );
  };

let rec deriv_3 = (all_chars, categories, cat, x, rem) =>
  switch (x) {
  |  E.TSeq(y, z, kind) =>
    let y' = deriv_4(all_chars, categories, cat, y, [(all_chars, [])]);
    deriv_seq(all_chars, categories, cat, kind, y', z, rem);
  |  E.TExp(marks, e) =>
    deriv_1(all_chars, categories, marks, cat, e, rem)
  | E.TMatch(_) => Cset.prepend(all_chars, [x], rem)
  }

and deriv_4 = (all_chars, categories, cat, l, rem) =>
  switch (l) {
  | [] => rem
  | [y, ...r] =>
    deriv_3(
      all_chars,
      categories,
      cat,
      y,
      deriv_4(all_chars, categories, cat, r, rem),
    )
  };

let deriv = (tbl_ref, all_chars, categories, st) => {
  let der =
    deriv_4(
      all_chars,
      categories,
      st.State.category,
      st.State.desc,
      [(all_chars, [])],
    );
  simpl_tr(
    List.fold_right(
      ((s, expr), rem) => {
        let (expr', _) = remove_duplicates([], expr, eps_expr);
        /*
         Format.eprintf "@[<3>@[%a@]: %a / %a@]@." Cset.print s print_state expr print_state expr';
         */
        let idx = free_index(tbl_ref, expr');
        let expr'' = set_idx(idx, expr');
        List.fold_right(
          ((cat', s'), rem) => {
            let s'' = Cset.inter(s, s');
            if (Cset.is_empty(s'')) {
              rem;
            } else {
              [(s'', State.mk(idx, cat', expr'')), ...rem];
            };
          },
          categories,
          rem,
        );
      },
      der,
      [],
    ),
  );
};

/****/

let flatten_match = m => {
  let ma = List.fold_left((ma, (i, _)) => max(ma, i), -1, m);
  let res = Array.make(ma + 1, -1);
  List.iter(((i, v)) => res[i] = v, m);
  res;
};

let status = s =>
  switch (s.State.status) {
  | Some(st) => st
  | None =>
    let st =
      switch (s.State.desc) {
      | [] => Failed
      | [E.TMatch(m), ..._] =>
         Match(flatten_match(m.Marks.marks), m.Marks.pmarks)
      | _ => Running
      };

    s.State.status = Some(st);
    st;
  };
