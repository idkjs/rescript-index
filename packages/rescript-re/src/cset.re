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

type c = int;
type t = list((c, c));

let rec union = (l, l') =>
  switch (l, l') {
  | (_, []) => l
  | ([], _) => l'
  | ([(c1, c2), ...r], [(c1', c2'), ...r']) =>
    if (c2 + 1 < c1') {
      [(c1, c2), ...union(r, l')];
    } else if (c2' + 1 < c1) {
      [(c1', c2'), ...union(l, r')];
    } else if (c2 < c2') {
      union(r, [(min(c1, c1'), c2'), ...r']);
    } else {
      union([(min(c1, c1'), c2), ...r], r');
    }
  };

let rec inter = (l, l') =>
  switch (l, l') {
  | (_, []) => []
  | ([], _) => []
  | ([(c1, c2), ...r], [(c1', c2'), ...r']) =>
    if (c2 < c1') {
      inter(r, l');
    } else if (c2' < c1) {
      inter(l, r');
    } else if (c2 < c2') {
      [(max(c1, c1'), c2), ...inter(r, l')];
    } else {
      [(max(c1, c1'), c2'), ...inter(l, r')];
    }
  };

let rec diff = (l, l') =>
  switch (l, l') {
  | (_, []) => l
  | ([], _) => []
  | ([(c1, c2), ...r], [(c1', c2'), ...r']) =>
    if (c2 < c1') {
      [(c1, c2), ...diff(r, l')];
    } else if (c2' < c1) {
      diff(l, r');
    } else {
      let r'' =
        if (c2' < c2) {
          [(c2' + 1, c2), ...r];
        } else {
          r;
        };
      if (c1 < c1') {
        [(c1, c1' - 1), ...diff(r'', r')];
      } else {
        diff(r'', r');
      };
    }
  };

let single = c => [(c, c)];

let add = (c, l) => union(single(c), l);

let seq = (c, c') =>
  if (c <= c') {
    [(c, c')];
  } else {
    [(c', c)];
  };

let rec offset = (o, l) =>
  switch (l) {
  | [] => []
  | [(c1, c2), ...r] => [(c1 + o, c2 + o), ...offset(o, r)]
  };

let empty = [];

let rec mem = (c: int, s) =>
  switch (s) {
  | [] => false
  | [(c1, c2), ...rem] =>
    if (c <= c2) {
      c >= c1;
    } else {
      mem(c, rem);
    }
  };

/****/

type hash = int;

let rec hash_rec =
  fun
  | [] => 0
  | [(i, j), ...r] => i + 13 * j + 257 * hash_rec(r);
let hash = l => hash_rec(l) land 0x3FFFFFFF;

/****/

let print_one = (ch, (c1, c2)) =>
  if (c1 == c2) {
    Js.log3(ch, "%d", c1);
  } else {
    Js.log4(ch, "%d-%d", c1, c2);
  };

let pp = (print_one);

let rec iter = (t, ~f) =>
  switch (t) {
  | [] => ()
  | [(x, y), ...xs] =>
    f(x, y);
    iter(xs, ~f);
  };

let one_char =
  fun
  | [(i, j)] when i == j => Some(i)
  | _ => None;

module CSetMap =
  Map.Make({
    type t = (int, list((int, int)));
    let compare = ((i, u), (j, v)) => {
      let c = compare(i, j);
      if (c != 0) {
        c;
      } else {
        compare(u, v);
      };
    };
  });

let fold_right = (t, ~init, ~f) => List.fold_right(f, t, init);

let csingle = c => single(Char.code(c));

let cany = [(0, 255)];

let is_empty =
  fun
  | [] => true
  | _ => false;

let rec prepend = (s, x, l) =>
  switch (s, l) {
  | ([], _) => l
  | (_r, []) => []
  | ([(_c, c'), ...r], [([(d, _d')], _x'), ..._r']) when c' < d =>
    prepend(r, x, l)
  | ([(c, c'), ...r], [([(d, d')], x'), ...r']) =>
    if (c <= d) {
      if (c' < d') {
        [
          ([(d, c')], x @ x'),
          ...prepend(r, x, [([(c' + 1, d')], x'), ...r']),
        ];
      } else {
        [([(d, d')], x @ x'), ...prepend(s, x, r')];
      };
    } else if (c > d') {
      [([(d, d')], x'), ...prepend(s, x, r')];
    } else {
      [
        ([(d, c - 1)], x'),
        ...prepend(s, x, [([(c, d')], x'), ...r']),
      ];
    }
  | _ => assert(false)
  };

let pick =
  fun
  | [] => invalid_arg("Re_cset.pick")
  | [(x, _), ..._] => x;
