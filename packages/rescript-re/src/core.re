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

let rec iter = (n, f, v) =>
  if (n == 0) {
    v;
  } else {
    iter(n - 1, f, f(v));
  };

/****/

let unknown = (-2);
let break = (-3);

type match_info =
  | Match(Group.t)
  | Failed
  | Running;

type state = {
  idx: int,
  /* Index of the current position in the position table.
     Not yet computed transitions point to a dummy state where
     [idx] is set to [unknown];
     If [idx] is set to [break] for states that either always
     succeed or always fail. */
  real_idx: int,
  /* The real index, in case [idx] is set to [break] */
  next: array(state),
  /* Transition table, indexed by color */
  mutable final: list((Category.t, (Automata.idx, Automata.status))),
  /* Mapping from the category of the next character to
     - the index where the next position should be saved
     - possibly, the list of marks (and the corresponding indices)
       corresponding to the best match */
  desc: Automata.State.t /* Description of this state of the automata */,
};

/* Automata (compiled regular expression) */
type re = {
  initial: Automata.expr,
  /* The whole regular expression */
  mutable initial_states: list((Category.t, state)),
  /* Initial states, indexed by initial category */
  colors: Bytes.t,
  /* Color table */
  color_repr: Bytes.t,
  /* Table from colors to one character of this color */
  ncolor: int,
  /* Number of colors. */
  lnl: int,
  /* Color of the last newline. -1 if unnecessary */
  tbl: Automata.working_area,
  /* Temporary table used to compute the first available index
     when computing a new state */
  states: Automata.State.Table.t(state),
  /* States of the deterministic automata */
  group_count: int /* Number of groups in the regular expression */,
};

let pp_re = (ch, re) => Automata.pp(ch, re.initial);

let print_re = pp_re;

/* Information used during matching */
type info = {
  re,
  /* The automata */
  colors: Bytes.t,
  /* Color table ([x.colors = x.re.colors])
     Shortcut used for performance reasons */
  mutable positions: array(int),
  /* Array of mark positions
     The mark are off by one for performance reasons */
  pos: int,
  /* Position where the match is started */
  last: int /* Position where the match should stop */,
};

/****/

let category = (re, ~color) =>
  if (color == (-1)) {
    Category.inexistant;
  } else if
    /* Special category for the last newline */
    (color == re.lnl) {
    Category.((lastnewline ++ newline) ++ not_letter);
  } else {
    Category.from_char(Bytes.get(re.color_repr, color));
  };

/****/

let dummy_next = [||];

let unknown_state = {
  idx: unknown,
  real_idx: 0,
  next: dummy_next,
  final: [],
  desc: Automata.State.dummy,
};

let mk_state = (ncol, desc) => {
  let break_state =
    switch (Automata.status(desc)) {
    | Automata.Running => false
    | Automata.Failed
    | Automata.Match(_) => true
    };

  {
    idx:
      if (break_state) {
        break;
      } else {
        desc.Automata.State.idx;
      },
    real_idx: desc.Automata.State.idx,
    next:
      if (break_state) {
        dummy_next;
      } else {
        Array.make(ncol, unknown_state);
      },
    final: [],
    desc,
  };
};

let find_state = (re, desc) =>
  try(Automata.State.Table.find(re.states, desc)) {
  | Not_found =>
    let st = mk_state(re.ncolor, desc);
    Automata.State.Table.add(re.states, desc, st);
    st;
  };

/**** Match with marks ****/

let delta = (info, cat, ~color, st) => {
  let desc = Automata.delta(info.re.tbl, cat, color, st.desc);
  let len = Array.length(info.positions);
  if (desc.Automata.State.idx == len && len > 0) {
    let pos = info.positions;
    info.positions = Array.make(2 * len, 0);
    Array.blit(pos, 0, info.positions, 0, len);
  };
  desc;
};

let validate = (info, s: string, ~pos, st) => {
  let color = Char.code(Bytes.get(info.colors, Char.code(s.[pos])));
  let cat = category(info.re, ~color);
  let desc' = delta(info, cat, ~color, st);
  let st' = find_state(info.re, desc');
  st.next[color] = st';
};

/*
 let rec loop info s pos st =
   if pos < info.last then
     let st' = st.next.(Char.code info.cols.[Char.code s.[pos]]) in
     let idx = st'.idx in
     if idx >= 0 then begin
       info.positions.(idx) <- pos;
       loop info s (pos + 1) st'
     end else if idx = break then begin
       info.positions.(st'.real_idx) <- pos;
       st'
     end else begin (* Unknown *)
       validate info s pos st;
       loop info s pos st
     end
   else
     st
 */

let rec loop = (info, s: string, ~pos, st) =>
  if (pos < info.last) {
    let st' = st.next[Char.code(Bytes.get(info.colors, Char.code(s.[pos])))];
    loop2(info, s, ~pos, st, st');
  } else {
    st;
  }

and loop2 = (info, s, ~pos, st, st') =>
  if (st'.idx >= 0) {
    let pos = pos + 1;
    if (pos < info.last) {
      /* It is important to place these reads before the write */
      /* But then, we don't have enough registers left to store the
         right position.  So, we store the position plus one. */
      let st'' = st'.next[Char.code(
                            Bytes.get(info.colors, Char.code(s.[pos])),
                          )];
      info.positions[st'.idx] = pos;
      loop2(info, s, ~pos, st', st'');
    } else {
      info.positions[st'.idx] = pos;
      st';
    };
  } else if (st'.idx == break) {
    info.positions[st'.real_idx] = pos + 1;
    st';
  } else {
    /* Unknown */
    validate(info, s, ~pos, st);
    loop(info, s, ~pos, st);
  };

let rec loop_no_mark = (info, s, ~pos, ~last, st) =>
  if (pos < last) {
    let st' = st.next[Char.code(Bytes.get(info.colors, Char.code(s.[pos])))];
    if (st'.idx >= 0) {
      loop_no_mark(info, s, ~pos=pos + 1, ~last, st');
    } else if (st'.idx == break) {
      st';
    } else {
      /* Unknown */
      validate(info, s, ~pos, st);
      loop_no_mark(info, s, ~pos, ~last, st);
    };
  } else {
    st;
  };

let final = (info, st, cat) =>
  try(List.assq(cat, st.final)) {
  | Not_found =>
    let st' = delta(info, cat, ~color=-1, st);
    let res = (st'.Automata.State.idx, Automata.status(st'));
    st.final = [(cat, res), ...st.final];
    res;
  };

let find_initial_state = (re, cat) =>
  try(List.assq(cat, re.initial_states)) {
  | Not_found =>
    let st = find_state(re, Automata.State.create(cat, re.initial));
    re.initial_states = [(cat, st), ...re.initial_states];
    st;
  };

let get_color = (re, s: string, pos) =>
  if (pos < 0) {
    (-1);
  } else {
    let slen = String.length(s);
    if (pos >= slen) {
      (-1);
    } else if (pos == slen - 1 && re.lnl != (-1) && s.[pos] == '\n') {
      /* Special case for the last newline */
      re.lnl;
    } else {
      Char.code(Bytes.get(re.colors, Char.code(s.[pos])));
    };
  };

let rec handle_last_newline = (info, ~pos, st, ~groups) => {
  let st' = st.next[info.re.lnl];
  if (st'.idx >= 0) {
    if (groups) {
      info.positions[st'.idx] = pos + 1;
    };
    st';
  } else if (st'.idx == break) {
    if (groups) {
      info.positions[st'.real_idx] = pos + 1;
    };
    st';
  } else {
    /* Unknown */
    let color = info.re.lnl;
    let real_c = Char.code(Bytes.get(info.colors, Char.code('\n')));
    let cat = category(info.re, ~color);
    let desc' = delta(info, cat, ~color=real_c, st);
    let st' = find_state(info.re, desc');
    st.next[color] = st';
    handle_last_newline(info, ~pos, st, ~groups);
  };
};

let rec scan_str = (info, s: string, initial_state, ~groups) => {
  let pos = info.pos;
  let last = info.last;
  if (last == String.length(s)
      && info.re.lnl != (-1)
      && last > pos
      && s.[last - 1] == '\n') {
    let info = {...info, last: last - 1};
    let st = scan_str(info, s, initial_state, ~groups);
    if (st.idx == break) {
      st;
    } else {
      handle_last_newline(info, ~pos=last - 1, st, ~groups);
    };
  } else if (groups) {
    loop(info, s, ~pos, initial_state);
  } else {
    loop_no_mark(info, s, ~pos, ~last, initial_state);
  };
};

let match_str = (~groups, ~partial, re, s, ~pos, ~len) => {
  let slen = String.length(s);
  let last =
    if (len == (-1)) {
      slen;
    } else {
      pos + len;
    };
  let info = {
    re,
    colors: re.colors,
    pos,
    last,
    positions:
      if (groups) {
        let n = Automata.index_count(re.tbl) + 1;
        if (n <= 10) {
          [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|];
        } else {
          Array.make(n, 0);
        };
      } else {
        [||];
      },
  };

  let initial_cat =
    if (pos == 0) {
      Category.(search_boundary ++ inexistant);
    } else {
      Category.(
        search_boundary ++ category(re, ~color=get_color(re, s, pos - 1))
      );
    };

  let initial_state = find_initial_state(re, initial_cat);
  let st = scan_str(info, s, initial_state, ~groups);
  let res =
    if (st.idx == break || partial) {
      Automata.status(st.desc);
    } else {
      let final_cat =
        if (last == slen) {
          Category.(search_boundary ++ inexistant);
        } else {
          Category.(
            search_boundary ++ category(re, ~color=get_color(re, s, last))
          );
        };

      let (idx, res) = final(info, st, final_cat);
      if (groups) {
        info.positions[idx] = last + 1;
      };
      res;
    };

  switch (res) {
  |  Automata.Match(marks, pmarks) =>
    Match({s, marks, pmarks, gpos: info.positions, gcount: re.group_count})
  | Automata.Failed => Failed
  | Automata.Running => Running
  };
};

let mk_re = (~initial, ~colors, ~color_repr, ~ncolor, ~lnl, ~group_count) => {
  initial,
  initial_states: [],
  colors,
  color_repr,
  ncolor,
  lnl,
  tbl: Automata.create_working_area(),
  states: Automata.State.Table.create(97),
  group_count,
};

/**** Character sets ****/

let cseq = (c, c') => Cset.seq(Char.code(c), Char.code(c'));
let cadd = (c, s) => Cset.add(Char.code(c), s);

let trans_set = (cache, cm, s) =>
  switch (Cset.one_char(s)) {
  | Some(i) => Cset.csingle(Bytes.get(cm, i))
  | None =>
    let v = (Cset.hash_rec(s), s);
    try(Cset.CSetMap.find(v, cache^)) {
    | Not_found =>
      let l =
        Cset.fold_right(
          s,
          ~f=
            ((i, j), l) =>
              Cset.union(cseq(Bytes.get(cm, i), Bytes.get(cm, j)), l),
          ~init=Cset.empty,
        );

      cache := Cset.CSetMap.add(v, l, cache^);
      l;
    };
  };

/****/

type regexp =
  | Set(Cset.t)
  | Sequence(list(regexp))
  | Alternative(list(regexp))
  | Repeat(regexp, int, option(int))
  | Beg_of_line
  | End_of_line
  | Beg_of_word
  | End_of_word
  | Not_bound
  | Beg_of_str
  | End_of_str
  | Last_end_of_line
  | Start
  | Stop
  | Sem(Automata.sem, regexp)
  | Sem_greedy(Automata.rep_kind, regexp)
  | Group(regexp)
  | No_group(regexp)
  | Nest(regexp)
  | Case(regexp)
  | No_case(regexp)
  | Intersection(list(regexp))
  | Complement(list(regexp))
  | Difference(regexp, regexp)
  | Pmark(Pmark.t, regexp);

module View = {
  type t =
    regexp =
      | Set(Cset.t)
      | Sequence(list(regexp))
      | Alternative(list(regexp))
      | Repeat(regexp, int, option(int))
      | Beg_of_line
      | End_of_line
      | Beg_of_word
      | End_of_word
      | Not_bound
      | Beg_of_str
      | End_of_str
      | Last_end_of_line
      | Start
      | Stop
      | Sem(Automata.sem, regexp)
      | Sem_greedy(Automata.rep_kind, regexp)
      | Group(regexp)
      | No_group(regexp)
      | Nest(regexp)
      | Case(regexp)
      | No_case(regexp)
      | Intersection(list(regexp))
      | Complement(list(regexp))
      | Difference(regexp, regexp)
      | Pmark(Pmark.t, regexp);

  let view = t => t;
};

let rec pp = (fmt, t) => {
  // open Fmt;
  let var = (s, re) => sexp(fmt, s, pp, re);
  let seq = (s, rel) => sexp(fmt, s, list(pp), rel);
  switch (t) {
  | Set(s) => sexp(fmt, "Set", Cset.pp, s)
  | Sequence(sq) => seq("Sequence", sq)
  | Alternative(alt) => seq("Alternative", alt)
  |  Repeat(re, start, stop) =>
    let pp' = (fmt, ()) =>
      fprintf(fmt, "%a@ %d%a", pp, re, start, optint, stop);
    sexp(fmt, "Repeat", pp', ());
  | Beg_of_line => str(fmt, "Beg_of_line")
  | End_of_line => str(fmt, "End_of_line")
  | Beg_of_word => str(fmt, "Beg_of_word")
  | End_of_word => str(fmt, "End_of_word")
  | Not_bound => str(fmt, "Not_bound")
  | Beg_of_str => str(fmt, "Beg_of_str")
  | End_of_str => str(fmt, "End_of_str")
  | Last_end_of_line => str(fmt, "Last_end_of_line")
  | Start => str(fmt, "Start")
  | Stop => str(fmt, "Stop")
  |  Sem(sem, re) =>
    sexp(fmt, "Sem", pair(Automata.pp_sem, pp), (sem, re))
  |  Sem_greedy(k, re) =>
    sexp(fmt, "Sem_greedy", pair(Automata.pp_rep_kind, pp), (k, re))
  | Group(c) => var("Group", c)
  | No_group(c) => var("No_group", c)
  | Nest(c) => var("Nest", c)
  | Case(c) => var("Case", c)
  | No_case(c) => var("No_case", c)
  | Intersection(c) => seq("Intersection", c)
  | Complement(c) => seq("Complement", c)
  |  Difference(a, b) =>
    sexp(fmt, "Difference", pair(pp, pp), (a, b))
  |  Pmark(m, r) =>
    sexp(fmt, "Pmark", pair(Pmark.pp, pp), (m, r))
  };
};

let rec is_charset =
  fun
  | Set(_) => true
  | Alternative(l)
  | Intersection(l)
  | Complement(l) => List.for_all(is_charset, l)
  |  Difference(r, r') => is_charset(r) && is_charset(r')
  |  Sem(_, r)
  |  Sem_greedy(_, r)
  | No_group(r)
  | Case(r)
  | No_case(r) => is_charset(r)
  | Sequence(_)
  | Repeat(_)
  | Beg_of_line
  | End_of_line
  | Beg_of_word
  | End_of_word
  | Beg_of_str
  | End_of_str
  | Not_bound
  | Last_end_of_line
  | Start
  | Stop
  | Group(_)
  | Nest(_)
  |  Pmark(_, _) => false;

/*XXX Use a better algorithm allowing non-contiguous regions? */

let cupper =
  Cset.union(
    cseq('A', 'Z'),
    Cset.union(cseq('\192', '\214'), cseq('\216', '\222')),
  );
let clower = Cset.offset(32, cupper);
let calpha =
  List.fold_right(
    cadd,
    ['\170', '\181', '\186', '\223', '\255'],
    Cset.union(clower, cupper),
  );
let cdigit = cseq('0', '9');
let calnum = Cset.union(calpha, cdigit);
let cword = cadd('_', calnum);

let colorize = (c, regexp) => {
  let lnl = ref(false);
  let rec colorize = regexp =>
    switch (regexp) {
    | Set(s) => Color_map.split(s, c)
    | Sequence(l) => List.iter(colorize, l)
    | Alternative(l) => List.iter(colorize, l)
    |  Repeat(r, _, _) => colorize(r)
    | Beg_of_line
    | End_of_line => Color_map.split(Cset.csingle('\n'), c)
    | Beg_of_word
    | End_of_word
    | Not_bound => Color_map.split(cword, c)
    | Beg_of_str
    | End_of_str
    | Start
    | Stop => ()
    | Last_end_of_line => lnl := true
    |  Sem(_, r)
    |  Sem_greedy(_, r)
    | Group(r)
    | No_group(r)
    | Nest(r)
    |  Pmark(_, r) => colorize(r)
    | Case(_)
    | No_case(_)
    | Intersection(_)
    | Complement(_)
    | Difference(_) => assert(false)
    };

  colorize(regexp);
  lnl^;
};

/**** Compilation ****/

let rec equal = (x1, x2) =>
  switch (x1, x2) {
  | (Set(s1), Set(s2)) => s1 == s2
  | (Sequence(l1), Sequence(l2)) => eq_list(l1, l2)
  | (Alternative(l1), Alternative(l2)) => eq_list(l1, l2)
  | (
       Repeat(x1', i1, j1),
       Repeat(x2', i2, j2),
    ) =>
    i1 == i2 && j1 == j2 && equal(x1', x2')
  | (Beg_of_line, Beg_of_line)
  | (End_of_line, End_of_line)
  | (Beg_of_word, Beg_of_word)
  | (End_of_word, End_of_word)
  | (Not_bound, Not_bound)
  | (Beg_of_str, Beg_of_str)
  | (End_of_str, End_of_str)
  | (Last_end_of_line, Last_end_of_line)
  | (Start, Start)
  | (Stop, Stop) => true
  | ( Sem(sem1, x1'),  Sem(sem2, x2')) =>
    sem1 == sem2 && equal(x1', x2')
  | (
       Sem_greedy(k1, x1'),
       Sem_greedy(k2, x2'),
    ) =>
    k1 == k2 && equal(x1', x2')
  | (Group(_), Group(_)) =>
    /* Do not merge groups! */
    false
  | (No_group(x1'), No_group(x2')) => equal(x1', x2')
  | (Nest(x1'), Nest(x2')) => equal(x1', x2')
  | (Case(x1'), Case(x2')) => equal(x1', x2')
  | (No_case(x1'), No_case(x2')) => equal(x1', x2')
  | (Intersection(l1), Intersection(l2)) => eq_list(l1, l2)
  | (Complement(l1), Complement(l2)) => eq_list(l1, l2)
  | (
       Difference(x1', x1''),
       Difference(x2', x2''),
    ) =>
    equal(x1', x2') && equal(x1'', x2'')
  | ( Pmark(m1, r1),  Pmark(m2, r2)) =>
    Pmark.equal(m1, m2) && equal(r1, r2)
  | _ => false
  }

and eq_list = (l1, l2) =>
  switch (l1, l2) {
  | ([], []) => true
  | ([x1, ...r1], [x2, ...r2]) => equal(x1, x2) && eq_list(r1, r2)
  | _ => false
  };

let sequence =
  fun
  | [x] => x
  | l => Sequence(l);

let rec merge_sequences =
  fun
  | [] => []
  | [Alternative(l'), ...r] => merge_sequences(l' @ r)
  | [Sequence([x, ...y]), ...r] =>
    switch (merge_sequences(r)) {
    | [Sequence([x', ...y']), ...r'] when equal(x, x') => [
        Sequence([x, Alternative([sequence(y), sequence(y')])]),
        ...r',
      ]
    | r' => [Sequence([x, ...y]), ...r']
    }
  | [x, ...r] => [x, ...merge_sequences(r)];

module A = Automata;

let enforce_kind = (ids, kind, kind', cr) =>
  switch (kind, kind') {
  | (`First, `First) => cr
  | (`First, k) => A.seq(ids, k, cr, A.eps(ids))
  | _ => cr
  };

/* XXX should probably compute a category mask */
let rec translate = (ids, kind, ign_group, ign_case, greedy, pos, cache, c) =>
  fun
  | Set(s) => (A.cst(ids, trans_set(cache, c, s)), kind)
  | Sequence(l) => (
      trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, l),
      kind,
    )
  | Alternative(l) =>
    switch (merge_sequences(l)) {
    | [r'] =>
      let (cr, kind') =
        translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r');
      (enforce_kind(ids, kind, kind', cr), kind);
    | merged_sequences => (
        A.alt(
          ids,
          List.map(
            r' => {
              let (cr, kind') =
                translate(
                  ids,
                  kind,
                  ign_group,
                  ign_case,
                  greedy,
                  pos,
                  cache,
                  c,
                  r',
                );
              enforce_kind(ids, kind, kind', cr);
            },
            merged_sequences,
          ),
        ),
        kind,
      )
    }
  |  Repeat(r', i, j) => {
      let (cr, kind') =
        translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r');
      let rem =
        switch (j) {
        | None => A.rep(ids, greedy, kind', cr)
        | Some(j) =>
          let f =
            switch (greedy) {
            | `Greedy => (
                rem =>
                  A.alt(
                    ids,
                    [
                      A.seq(ids, kind', A.rename(ids, cr), rem),
                      A.eps(ids),
                    ],
                  )
              )
            | `Non_greedy => (
                rem =>
                  A.alt(
                    ids,
                    [
                      A.eps(ids),
                      A.seq(ids, kind', A.rename(ids, cr), rem),
                    ],
                  )
              )
            };

          iter(j - i, f, A.eps(ids));
        };

      (
        iter(i, rem => A.seq(ids, kind', A.rename(ids, cr), rem), rem),
        kind,
      );
    }
  | Beg_of_line => (A.after(ids, Category.(inexistant ++ newline)), kind)
  | End_of_line => (A.before(ids, Category.(inexistant ++ newline)), kind)
  | Beg_of_word => (
      A.seq(
        ids,
        `First,
        A.after(ids, Category.(inexistant ++ not_letter)),
        A.before(ids, Category.(inexistant ++ letter)),
      ),
      kind,
    )
  | End_of_word => (
      A.seq(
        ids,
        `First,
        A.after(ids, Category.(inexistant ++ letter)),
        A.before(ids, Category.(inexistant ++ not_letter)),
      ),
      kind,
    )
  | Not_bound => (
      A.alt(
        ids,
        [
          A.seq(
            ids,
            `First,
            A.after(ids, Category.letter),
            A.before(ids, Category.letter),
          ),
          A.seq(
            ids,
            `First,
            A.after(ids, Category.letter),
            A.before(ids, Category.letter),
          ),
        ],
      ),
      kind,
    )
  | Beg_of_str => (A.after(ids, Category.inexistant), kind)
  | End_of_str => (A.before(ids, Category.inexistant), kind)
  | Last_end_of_line => (
      A.before(ids, Category.(inexistant ++ lastnewline)),
      kind,
    )
  | Start => (A.after(ids, Category.search_boundary), kind)
  | Stop => (A.before(ids, Category.search_boundary), kind)
  |  Sem(kind', r') => {
      let (cr, kind'') =
        translate(ids, kind', ign_group, ign_case, greedy, pos, cache, c, r');
      (enforce_kind(ids, kind', kind'', cr), kind');
    }
  |  Sem_greedy(greedy', r') =>
    translate(ids, kind, ign_group, ign_case, greedy', pos, cache, c, r')
  | Group(r') =>
    if (ign_group) {
      translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r');
    } else {
      let p = pos^;
      pos := pos^ + 2;
      let (cr, kind') =
        translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r');
      (
        A.seq(
          ids,
          `First,
          A.mark(ids, p),
          A.seq(ids, `First, cr, A.mark(ids, p + 1)),
        ),
        kind',
      );
    }
  | No_group(r') =>
    translate(ids, kind, true, ign_case, greedy, pos, cache, c, r')
  | Nest(r') => {
      let b = pos^;
      let (cr, kind') =
        translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r');

      let e = pos^ - 1;
      if (e < b) {
        (cr, kind');
      } else {
        (A.seq(ids, `First, A.erase(ids, b, e), cr), kind');
      };
    }
  | Difference(_)
  | Complement(_)
  | Intersection(_)
  | No_case(_)
  | Case(_) => assert(false)
  |  Pmark(i, r') => {
      let (cr, kind') =
        translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r');
      (A.seq(ids, `First, A.pmark(ids, i), cr), kind');
    }

and trans_seq = (ids, kind, ign_group, ign_case, greedy, pos, cache, c) =>
  fun
  | [] => A.eps(ids)
  | [r] => {
      let (cr', kind') =
        translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r);
      enforce_kind(ids, kind, kind', cr');
    }
  | [r, ...rem] => {
      let (cr', kind') =
        translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r);
      let cr'' =
        trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, rem);
      if (A.is_eps(cr'')) {
        cr';
      } else if (A.is_eps(cr')) {
        cr'';
      } else {
        A.seq(ids, kind', cr', cr'');
      };
    };

/**** Case ****/

let case_insens = s =>
  Cset.union(
    s,
    Cset.union(
      Cset.offset(32, Cset.inter(s, cupper)),
      Cset.offset(-32, Cset.inter(s, clower)),
    ),
  );

let as_set =
  fun
  | Set(s) => s
  | _ => assert(false);

/* XXX Should split alternatives into (1) charsets and (2) more
   complex regular expressions; alternative should therefore probably
   be flatten here */
let rec handle_case = ign_case =>
  fun
  | Set(s) =>
    Set(
      if (ign_case) {
        case_insens(s);
      } else {
        s;
      },
    )
  | Sequence(l) => Sequence(List.map(handle_case(ign_case), l))
  | Alternative(l) => {
      let l' = List.map(handle_case(ign_case), l);
      if (is_charset(Alternative(l'))) {
        Set(
          List.fold_left(
            (s, r) => Cset.union(s, as_set(r)),
            Cset.empty,
            l',
          ),
        );
      } else {
        Alternative(l');
      };
    }
  |  Repeat(r, i, j) =>
     Repeat(handle_case(ign_case, r), i, j)
  | (
      Beg_of_line | End_of_line | Beg_of_word | End_of_word | Not_bound |
      Beg_of_str |
      End_of_str |
      Last_end_of_line |
      Start |
      Stop
    ) as r => r
  |  Sem(k, r) => {
      let r' = handle_case(ign_case, r);
      if (is_charset(r')) {
        r';
      } else {
         Sem(k, r');
      };
    }
  |  Sem_greedy(k, r) => {
      let r' = handle_case(ign_case, r);
      if (is_charset(r')) {
        r';
      } else {
         Sem_greedy(k, r');
      };
    }
  | Group(r) => Group(handle_case(ign_case, r))
  | No_group(r) => {
      let r' = handle_case(ign_case, r);
      if (is_charset(r')) {
        r';
      } else {
        No_group(r');
      };
    }
  | Nest(r) => {
      let r' = handle_case(ign_case, r);
      if (is_charset(r')) {
        r';
      } else {
        Nest(r');
      };
    }
  | Case(r) => handle_case(false, r)
  | No_case(r) => handle_case(true, r)
  | Intersection(l) => {
      let l' = List.map(r => handle_case(ign_case, r), l);
      Set(
        List.fold_left((s, r) => Cset.inter(s, as_set(r)), Cset.cany, l'),
      );
    }
  | Complement(l) => {
      let l' = List.map(r => handle_case(ign_case, r), l);
      Set(
        Cset.diff(
          Cset.cany,
          List.fold_left(
            (s, r) => Cset.union(s, as_set(r)),
            Cset.empty,
            l',
          ),
        ),
      );
    }
  |  Difference(r, r') =>
    Set(
      Cset.inter(
        as_set(handle_case(ign_case, r)),
        Cset.diff(Cset.cany, as_set(handle_case(ign_case, r'))),
      ),
    )
  |  Pmark(i, r) =>
     Pmark(i, handle_case(ign_case, r));

/****/

let compile_1 = regexp => {
  let regexp = handle_case(false, regexp);
  let c = Color_map.make();
  let need_lnl = colorize(c, regexp);
  let (colors, color_repr, ncolor) = Color_map.flatten(c);
  let lnl = if (need_lnl) {ncolor} else {(-1)};
  let ncolor =
    if (need_lnl) {
      ncolor + 1;
    } else {
      ncolor;
    };
  let ids = A.create_ids();
  let pos = ref(0);
  let (r, kind) =
    translate(
      ids,
      `First,
      false,
      false,
      `Greedy,
      pos,
      ref(Cset.CSetMap.empty),
      colors,
      regexp,
    );
  let r = enforce_kind(ids, `First, kind, r);
  /*Format.eprintf "<%d %d>@." !ids ncol;*/
  mk_re(
    ~initial=r,
    ~colors,
    ~color_repr,
    ~ncolor,
    ~lnl,
    ~group_count=pos^ / 2,
  );
};

/****/

let rec anchored =
  fun
  | Sequence(l) => List.exists(anchored, l)
  | Alternative(l) => List.for_all(anchored, l)
  |  Repeat(r, i, _) => i > 0 && anchored(r)
  | Set(_)
  | Beg_of_line
  | End_of_line
  | Beg_of_word
  | End_of_word
  | Not_bound
  | End_of_str
  | Last_end_of_line
  | Stop
  | Intersection(_)
  | Complement(_)
  | Difference(_) => false
  | Beg_of_str
  | Start => true
  |  Sem(_, r)
  |  Sem_greedy(_, r)
  | Group(r)
  | No_group(r)
  | Nest(r)
  | Case(r)
  | No_case(r)
  |  Pmark(_, r) => anchored(r);

/****/

type t = regexp;

let str = s => {
  let l = ref([]);
  for (i in String.length(s) - 1 downto 0) {
    l := [Set(Cset.csingle(s.[i])), ...l^];
  };
  Sequence(l^);
};
let char = c => Set(Cset.csingle(c));

let alt =
  fun
  | [r] => r
  | l => Alternative(l);
let seq =
  fun
  | [r] => r
  | l => Sequence(l);

let empty = alt([]);
let epsilon = seq([]);
let repn = (r, i, j) => {
  if (i < 0) {
    invalid_arg("Re.repn");
  };
  switch (j) {
  | Some(j) when j < i => invalid_arg("Re.repn")
  | _ => ()
  };
   Repeat(r, i, j);
};
let rep = r => repn(r, 0, None);
let rep1 = r => repn(r, 1, None);
let opt = r => repn(r, 0, Some(1));
let bol = Beg_of_line;
let eol = End_of_line;
let bow = Beg_of_word;
let eow = End_of_word;
let word = r => seq([bow, r, eow]);
let not_boundary = Not_bound;
let bos = Beg_of_str;
let eos = End_of_str;
let whole_string = r => seq([bos, r, eos]);
let leol = Last_end_of_line;
let start = Start;
let stop = Stop;
let longest = r =>  Sem(`Longest, r);
let shortest = r =>  Sem(`Shortest, r);
let first = r =>  Sem(`First, r);
let greedy = r =>  Sem_greedy(`Greedy, r);
let non_greedy = r =>  Sem_greedy(`Non_greedy, r);
let group = r => Group(r);
let no_group = r => No_group(r);
let nest = r => Nest(r);
let mark = r => {
  let i = Pmark.gen();
  (i,  Pmark(i, r));
};

let set = str => {
  let s = ref(Cset.empty);
  for (i in 0 to String.length(str) - 1) {
    s := Cset.union(Cset.csingle(str.[i]), s^);
  };
  Set(s^);
};

let rg = (c, c') => Set(cseq(c, c'));

let inter = l => {
  let r = Intersection(l);
  if (is_charset(r)) {
    r;
  } else {
    invalid_arg("Re.inter");
  };
};

let compl = l => {
  let r = Complement(l);
  if (is_charset(r)) {
    r;
  } else {
    invalid_arg("Re.compl");
  };
};

let diff = (r, r') => {
  let r'' =  Difference(r, r');
  if (is_charset(r'')) {
    r'';
  } else {
    invalid_arg("Re.diff");
  };
};

let any = Set(Cset.cany);
let notnl = Set(Cset.diff(Cset.cany, Cset.csingle('\n')));

let lower =
  alt([
    rg('a', 'z'),
    char('\181'),
    rg('\223', '\246'),
    rg('\248', '\255'),
  ]);
let upper = alt([rg('A', 'Z'), rg('\192', '\214'), rg('\216', '\222')]);
let alpha = alt([lower, upper, char('\170'), char('\186')]);
let digit = rg('0', '9');
let alnum = alt([alpha, digit]);
let wordc = alt([alnum, char('_')]);
let ascii = rg('\000', '\127');
let blank = set("\t ");
let cntrl = alt([rg('\000', '\031'), rg('\127', '\159')]);
let graph = alt([rg('!', '~'), rg('\160', '\255')]);
let print = alt([rg(' ', '~'), rg('\160', '\255')]);
let punct =
  alt([
    rg('!', '/'),
    rg(':', '@'),
    rg('[', '`'),
    rg('{', '~'),
    rg('\160', '\169'),
    rg('\171', '\180'),
    rg('\182', '\185'),
    rg('\187', '\191'),
    char('\215'),
    char('\247'),
  ]);
let space = alt([char(' '), rg('\t', '\r')]);
let xdigit = alt([digit, rg('a', 'f'), rg('A', 'F')]);

let case = r => Case(r);
let no_case = r => No_case(r);

/****/

let compile = r =>
  compile_1(
    if (anchored(r)) {
      group(r);
    } else {
      seq([shortest(rep(any)), group(r)]);
    },
  );

let exec_internal = (name, ~pos=0, ~len=(-1), ~partial, ~groups, re, s) => {
  if (pos < 0 || len < (-1) || pos + len > String.length(s)) {
    invalid_arg(name);
  };
  match_str(~groups, ~partial, re, s, ~pos, ~len);
};

let exec = (~pos=?, ~len=?, re, s) =>
  switch (
    exec_internal(
      "Re.exec",
      ~pos?,
      ~len?,
      ~groups=true,
      ~partial=false,
      re,
      s,
    )
  ) {
  | Match(substr) => substr
  | _ => raise(Not_found)
  };

let exec_opt = (~pos=?, ~len=?, re, s) =>
  switch (
    exec_internal(
      "Re.exec_opt",
      ~pos?,
      ~len?,
      ~groups=true,
      ~partial=false,
      re,
      s,
    )
  ) {
  | Match(substr) => Some(substr)
  | _ => None
  };

let execp = (~pos=?, ~len=?, re, s) =>
  switch (
    exec_internal(
      ~groups=false,
      ~partial=false,
      "Re.execp",
      ~pos?,
      ~len?,
      re,
      s,
    )
  ) {
  | Match(_substr) => true
  | _ => false
  };

let exec_partial = (~pos=?, ~len=?, re, s) =>
  switch (
    exec_internal(
      ~groups=false,
      ~partial=true,
      "Re.exec_partial",
      ~pos?,
      ~len?,
      re,
      s,
    )
  ) {
  | Match(_) => `Full
  | Running => `Partial
  | Failed => `Mismatch
  };

module Mark = {
  type t = Pmark.t;

  let test = (g: Group.t, p) => Pmark.Set.mem(p, g.pmarks);

  let all = (g: Group.t) => g.pmarks;

  module Set = Pmark.Set;

  let equal = Pmark.equal;

  let compare = Pmark.compare;
};

type split_token = [ | `Text(string) | `Delim(Group.t)];

module Rseq = {
  let all = (~pos=0, ~len=?, re, s): Seq.t(_) => {
    if (pos < 0) {
      invalid_arg("Re.all");
    };
    /* index of the first position we do not consider.
       !pos < limit is an invariant */
    let limit =
      switch (len) {
      | None => String.length(s)
      | Some(l) =>
        if (l < 0 || pos + l > String.length(s)) {
          invalid_arg("Re.all");
        };
        pos + l;
      };

    /* iterate on matches. When a match is found, search for the next
       one just after its end */
    let rec aux = (pos, ()) =>
      if (pos >= limit) {
        Seq.Nil;
      } else {
        /* no more matches */

        switch (
          match_str(
            ~groups=true,
            ~partial=false,
            re,
            s,
            ~pos,
            ~len=limit - pos,
          )
        ) {
        | Match(substr) =>
          let (p1, p2) = Group.offset(substr, 0);
          let pos =
            if (p1 == p2) {
              p2 + 1;
            } else {
              p2;
            };
           Seq.Cons(substr, aux(pos));
        | Running
        | Failed => Seq.Nil
        };
      };

    aux(pos);
  };

  let matches = (~pos=?, ~len=?, re, s): Seq.t(_) =>
    all(~pos?, ~len?, re, s) |> Seq.map(sub => Group.get(sub, 0));

  let split_full = (~pos=0, ~len=?, re, s): Seq.t(_) => {
    if (pos < 0) {
      invalid_arg("Re.split");
    };
    let limit =
      switch (len) {
      | None => String.length(s)
      | Some(l) =>
        if (l < 0 || pos + l > String.length(s)) {
          invalid_arg("Re.split");
        };
        pos + l;
      };

    /* i: start of delimited string
       pos: first position after last match of [re]
       limit: first index we ignore (!pos < limit is an invariant) */
    let pos0 = pos;
    let rec aux = (state, i, pos, ()) =>
      switch (state) {
      | `Idle when pos >= limit =>
        if (i < limit) {
          let sub = String.sub(s, i, limit - i);
           Seq.Cons(`Text(sub), aux(state, i + 1, pos));
        } else {
          Seq.Nil;
        }
      | `Idle =>
        switch (
          match_str(
            ~groups=true,
            ~partial=false,
            re,
            s,
            ~pos,
            ~len=limit - pos,
          )
        ) {
        | Match(substr) =>
          let (p1, p2) = Group.offset(substr, 0);
          let pos =
            if (p1 == p2) {
              p2 + 1;
            } else {
              p2;
            };
          let old_i = i;
          let i = p2;
          if (p1 > pos0) {
            /* string does not start by a delimiter */
            let text = String.sub(s, old_i, p1 - old_i);
            let state = `Yield(`Delim(substr));
             Seq.Cons(`Text(text), aux(state, i, pos));
          } else {
             Seq.Cons(`Delim(substr), aux(state, i, pos));
          };
        | Running => Seq.Nil
        | Failed =>
          if (i < limit) {
            let text = String.sub(s, i, limit - i);
            /* yield last string */
             Seq.Cons(`Text(text), aux(state, limit, pos));
          } else {
            Seq.Nil;
          }
        }
      | `Yield(x) =>  Seq.Cons(x, aux(`Idle, i, pos))
      };

    aux(`Idle, pos, pos);
  };

  let split = (~pos=?, ~len=?, re, s): Seq.t(_) => {
    let seq = split_full(~pos?, ~len?, re, s);
    let rec filter = (seq, ()) =>
      switch (seq()) {
      | Seq.Nil => Seq.Nil
      |  Seq.Cons(`Delim(_), tl) => filter(tl, ())
      |  Seq.Cons(`Text(s), tl) =>
         Seq.Cons(s, filter(tl))
      };
    filter(seq);
  };
};

module Rlist = {
  let list_of_seq = (s: Seq.t('a)): list('a) =>
    Seq.fold_left((l, x) => [x, ...l], [], s) |> List.rev;

  let all = (~pos=?, ~len=?, re, s) =>
    Rseq.all(~pos?, ~len?, re, s) |> list_of_seq;

  let matches = (~pos=?, ~len=?, re, s) =>
    Rseq.matches(~pos?, ~len?, re, s) |> list_of_seq;

  let split_full = (~pos=?, ~len=?, re, s) =>
    Rseq.split_full(~pos?, ~len?, re, s) |> list_of_seq;

  let split = (~pos=?, ~len=?, re, s) =>
    Rseq.split(~pos?, ~len?, re, s) |> list_of_seq;
};

module Gen = {
  type gen('a) = unit => option('a);
  let gen_of_seq = (s: Seq.t('a)): gen('a) => {
    let r = ref(s);
    () =>
      switch (r^()) {
      | Seq.Nil => None
      |  Seq.Cons(x, tl) =>
        r := tl;
        Some(x);
      };
  };

  let split = (~pos=?, ~len=?, re, s): gen(_) =>
    Rseq.split(~pos?, ~len?, re, s) |> gen_of_seq;

  let split_full = (~pos=?, ~len=?, re, s): gen(_) =>
    Rseq.split_full(~pos?, ~len?, re, s) |> gen_of_seq;

  let all = (~pos=?, ~len=?, re, s) =>
    Rseq.all(~pos?, ~len?, re, s) |> gen_of_seq;

  let matches = (~pos=?, ~len=?, re, s) =>
    Rseq.matches(~pos?, ~len?, re, s) |> gen_of_seq;
};

let replace = (~pos=0, ~len=?, ~all=true, re, ~f, s) => {
  if (pos < 0) {
    invalid_arg("Re.replace");
  };
  let limit =
    switch (len) {
    | None => String.length(s)
    | Some(l) =>
      if (l < 0 || pos + l > String.length(s)) {
        invalid_arg("Re.replace");
      };
      pos + l;
    };

  /* buffer into which we write the result */
  let buf = Buffer.create(String.length(s));
  /* iterate on matched substrings. */
  let rec iter = pos =>
    if (pos < limit) {
      switch (
        match_str(~groups=true, ~partial=false, re, s, ~pos, ~len=limit - pos)
      ) {
      | Match(substr) =>
        let (p1, p2) = Group.offset(substr, 0);
        /* add string between previous match and current match */
        Buffer.add_substring(buf, s, pos, p1 - pos);
        /* what should we replace the matched group with? */
        let replacing = f(substr);
        Buffer.add_string(buf, replacing);
        if (all) {
          /* if we matched a non-char e.g. ^ we must manually advance by 1 */
          iter(
            if (p1 == p2) {
              /* a non char could be past the end of string. e.g. $ */
              if (p2 < limit) {
                Buffer.add_char(buf, s.[p2]);
              };
              p2 + 1;
            } else {
              p2;
            },
          );
        } else {
          Buffer.add_substring(buf, s, p2, limit - p2);
        };
      | Running => ()
      | Failed => Buffer.add_substring(buf, s, pos, limit - pos)
      };
    };

  iter(pos);
  Buffer.contents(buf);
};

let replace_string = (~pos=?, ~len=?, ~all=?, re, ~by, s) =>
  replace(~pos?, ~len?, ~all?, re, s, ~f=_ => by);

let witness = t => {
  let rec witness =
    fun
    | Set(c) => String.make(1, Char.chr(Cset.pick(c)))
    | Sequence(xs) => String.concat("", List.map(witness, xs))
    | Alternative([x, ..._]) => witness(x)
    | Alternative([]) => assert(false)
    |  Repeat(r, from, _to) => {
        let w = witness(r);
        let b = Buffer.create(String.length(w) * from);
        for (_i in 1 to from) {
          Buffer.add_string(b, w);
        };
        Buffer.contents(b);
      }
    | No_case(r) => witness(r)
    | Intersection(_)
    | Complement(_)
    |  Difference(_, _) => assert(false)
    | Group(r)
    | No_group(r)
    | Nest(r)
    |  Sem(_, r)
    |  Pmark(_, r)
    | Case(r)
    |  Sem_greedy(_, r) => witness(r)
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | Last_end_of_line
    | Start
    | Stop
    | End_of_str => "";
  witness(handle_case(false, t));
};

module Seq = Rseq;
module List = Rlist;
module Group = Group;

/** {2 Deprecated functions} */;

type gen('a) = Gen.gen('a);
let all_gen = Gen.all;
let matches_gen = Gen.matches;
let split_gen = Gen.split;
let split_full_gen = Gen.split_full;

type substrings = Group.t;

let get = Group.get;
let get_ofs = Group.offset;
let get_all = Group.all;
let get_all_ofs = Group.all_offset;
let test = Group.test;

type markid = Mark.t;

let marked = Mark.test;
let mark_set = Mark.all;

/**********************************/

/*
 Information about the previous character:
 - does not exists
 - is a letter
 - is not a letter
 - is a newline
 - is last newline

 Beginning of word:
 - previous is not a letter or does not exist
 - current is a letter or does not exist

 End of word:
 - previous is a letter or does not exist
 - current is not a letter or does not exist

 Beginning of line:
 - previous is a newline or does not exist

 Beginning of buffer:
 - previous does not exist

 End of buffer
 - current does not exist

 End of line
 - current is a newline or does not exist
 */

/*
 Rep: e = T,e | ()
   - semantics of the comma (shortest/longest/first)
   - semantics of the union (greedy/non-greedy)

 Bounded repetition
   a{0,3} = (a,(a,a?)?)?
 */

type groups = Group.t;

include Rlist;
