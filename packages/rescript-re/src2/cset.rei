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

/* Character sets, represented as sorted list of intervals */

type c = int;
type t;

let iter: (t, ~f: (c, c) => unit) => unit;

let union: (t, t) => t;
let inter: (t, t) => t;
let diff: (t, t) => t;
let offset: (int, t) => t;

let empty: t;
let single: c => t;
let seq: (c, c) => t;
let add: (c, t) => t;

let mem: (c, t) => bool;

type hash;
let hash: t => hash;

let pp: (unit, t) => unit;

let one_char: t => option(c);

let fold_right: (t, ~init: 'acc, ~f: ((c, c), 'acc) => 'acc) => 'acc;

let hash_rec: t => int;

module CSetMap: Map.S with type key = (int, t);

let cany: t;

let csingle: char => t;

let is_empty: t => bool;

let prepend: (t, list('a), list((t, list('a)))) => list((t, list('a)));

let pick: t => c;
