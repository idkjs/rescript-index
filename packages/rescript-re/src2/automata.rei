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

/* Regular expressions */

type mark = int;

type sem = [ | `Longest | `Shortest | `First];
type rep_kind = [ | `Greedy | `Non_greedy];

let pp_sem: (sem) => unit;
let pp_rep_kind: (unit, rep_kind) => unit;

type expr;
let is_eps: expr => bool;
let pp: (string, expr) => unit;

type ids;
let create_ids: unit => ids;

let cst: (ids, Cset.t) => expr;
let empty: ids => expr;
let alt: (ids, list(expr)) => expr;
let seq: (ids, sem, expr, expr) => expr;
let eps: ids => expr;
let rep: (ids, rep_kind, sem, expr) => expr;
let mark: (ids, mark) => expr;
let pmark: (ids, Pmark.t) => expr;
let erase: (ids, mark, mark) => expr;
let before: (ids, Category.t) => expr;
let after: (ids, Category.t) => expr;

let rename: (ids, expr) => expr;

/****/

/* States of the automata */

type idx = int;
module Marks: {
  type t = {
    marks: list((mark, idx)),
    pmarks: Pmark.Set.t,
  };
};

module E: {
  type t;
  let pp: (string, t) => unit;
};

type hash;
type mark_infos = array(int);
type status =
  | Failed
  | Match(mark_infos, Pmark.Set.t)
  | Running;

module State: {
  type t = {
    idx,
    category: Category.t,
    desc: list(E.t),
    mutable status: option(status),
    hash,
  };
  let dummy: t;
  let create: (Category.t, expr) => t;
  module Table: Hashtbl.S with type key = t;
};

/****/

/* Computation of the states following a given state */

type working_area;
let create_working_area: unit => working_area;
let index_count: working_area => int;

let delta: (working_area, Category.t, Cset.c, State.t) => State.t;
let deriv:
  (working_area, Cset.t, list((Category.t, Cset.t)), State.t) =>
  list((Cset.t, State.t));

/****/

let status: State.t => status;
