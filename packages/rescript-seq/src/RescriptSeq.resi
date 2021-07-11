@@ocaml.doc(
  /* ************************************************************************ */
  /*  */
  /* OCaml */
  /*  */
  /* Simon Cruanes */
  /*  */
  /* Copyright 2017 Institut National de Recherche en Informatique et */
  /* en Automatique. */
  /*  */
  /* All rights reserved.  This file is distributed under the terms of */
  /* the GNU Lesser General Public License version 2.1, with the */
  /* special exception on linking described in the file LICENSE. */
  /*  */
  /* ************************************************************************ */

  /* Module [Seq]: functional iterators */

  " {1 Functional Iterators} "
)

@@ocaml.doc(" The type ['a t] is a {b delayed list}, i.e. a list where some evaluation
    is needed to access the next element. This makes it possible to build
    infinite sequences, to build sequences as we traverse them, and to transform
    them in a lazy fashion rather than upfront.
")

@@ocaml.doc(" @since 4.07 ")

@ocaml.doc(" The type of delayed lists containing elements of type ['a].
    Note that the concrete list node ['a node] is delayed under a closure,
    not a [lazy] block, which means it might be recomputed every time
    we access it. ")
type rec t<'a> = unit => node<'a>

and node<+'a> =
  | Nil
  | @ocaml.doc(" A fully-evaluated list node, either empty or containing an element
    and a delayed tail. ")
  Cons('a, t<'a>)

@ocaml.doc(" The empty sequence, containing no elements. ")
let empty: t<'a>

@ocaml.doc(" The singleton sequence containing only the given element. ")
let return: 'a => t<'a>

@ocaml.doc(" [map f seq] returns a new sequence whose elements are the elements of
    [seq], transformed by [f].
    This transformation is lazy, it only applies when the result is traversed.
    If [seq = [1;2;3]], then [map f seq = [f 1; f 2; f 3]]. ")
let map: ('a => 'b, t<'a>) => t<'b>

@ocaml.doc(" Remove from the sequence the elements that do not satisfy the
    given predicate.
    This transformation is lazy, it only applies when the result is
    traversed. ")
let filter: ('a => bool, t<'a>) => t<'a>

@ocaml.doc(" Apply the function to every element; if [f x = None] then [x] is dropped;
    if [f x = Some y] then [y] is returned.
    This transformation is lazy, it only applies when the result is
    traversed. ")
let filter_map: ('a => option<'b>, t<'a>) => t<'b>

@ocaml.doc(" Map each element to a subsequence, then return each element of this
    sub-sequence in turn.
    This transformation is lazy, it only applies when the result is
    traversed. ")
let flat_map: ('a => t<'b>, t<'a>) => t<'b>

@ocaml.doc(" Traverse the sequence from left to right, combining each element with the
    accumulator using the given function.
    The traversal happens immediately and will not terminate on infinite
    sequences.
    Also see {!List.fold_left} ")
let fold_left: (('a, 'b) => 'a, 'a, t<'b>) => 'a

@ocaml.doc(" Iterate on the sequence, calling the (imperative) function on every element.
    The traversal happens immediately and will not terminate on infinite
    sequences. ")
let iter: ('a => unit, t<'a>) => unit
