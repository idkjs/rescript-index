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

/** Module [Re]: code for creating and using regular expressions,
   independently of regular expression syntax. */;

/** Regular expression */

type t;

/** Compiled regular expression */

type re;

/** Manipulate matching groups. */

module Group: {
  /** Information about groups in a match. As is conventional, every
      match implicitly has a group 0 that covers the whole match, and
      explicit groups are numbered from 1. */

  type t;

  /** Raise [Not_found] if the group did not match */

  let get: (t, int) => string;

  /** Similar to {!get}, but returns an option instead of using an exception. */

  let get_opt: (t, int) => option(string);

  /** Raise [Not_found] if the group did not match */

  let offset: (t, int) => (int, int);

  /** Return the start of the match. Raise [Not_found] if the group did not match. */

  let start: (t, int) => int;

  /** Return the end of the match. Raise [Not_found] if the group did not match. */

  let stop: (t, int) => int;

  /** Return the empty string for each group which did not match */

  let all: t => array(string);

  /** Return [(-1,-1)] for each group which did not match */

  let all_offset: t => array((int, int));

  /** Test whether a group matched */

  let test: (t, int) => bool;

  /** Returns the total number of groups defined - matched or not.
      This function is experimental. */

  let nb_groups: t => int;

  let pp: (Format.formatter, t) => unit;
};
[@ocaml.deprecated "Use Group.t"]
type groups = Group.t;

/** {2 Compilation and execution of a regular expression} */;

/** Compile a regular expression into an executable version that can be
    used to match strings, e.g. with {!exec}. */

let compile: t => re;

/** [exec re str] searches [str] for a match of the compiled expression [re],
    and returns the matched groups if any.

    More specifically, when a match exists, [exec] returns a match that
    starts at the earliest position possible. If multiple such matches are
    possible, the one specified by the match semantics described below is
    returned.

    @param pos optional beginning of the string (default 0)
    @param len length of the substring of [str] that can be matched (default [-1],
      meaning to the end of the string)
    @raise Not_found if the regular expression can't be found in [str]

    Note that [exec re str ~pos ~len] is not equivalent to [exec re
    (String.sub str pos len)]. This transformation changes the meaning
    of some constructs ({!bos}, {!eos}, {!whole_string} and {!leol}), and
    zero-width assertions like {!bow} or {!eow} look at characters before
    [pos] and after [pos + len].
*/

let exec:
  (
    ~pos: int=?, /*** Default: 0 */
    ~len: int=?, /*** Default: -1 (until end of string) */
    re,
    string
  ) =>
  Group.t;

/** Similar to {!exec}, but returns an option instead of using an exception. */

let exec_opt:
  (
    ~pos: int=?, /*** Default: 0 */
    ~len: int=?, /*** Default: -1 (until end of string) */
    re,
    string
  ) =>
  option(Group.t);

/** Similar to {!exec}, but returns [true] if the expression matches,
    and [false] if it doesn't. This function is more efficient than
    calling {!exec} or {!exec_opt} and ignoring the returned group.
 */

let execp:
  (
    ~pos: int=?, /*** Default: 0 */
    ~len: int=?, /*** Default: -1 (until end of string) */
    re,
    string
  ) =>
  bool;

/** More detailed version of {!exec_p}. [`Full] is equivalent to [true],
   while [`Mismatch] and [`Partial] are equivalent to [false], but [`Partial]
   indicates the input string could be extended to create a match. */

let exec_partial:
  (
    ~pos: int=?, /*** Default: 0 */
    ~len: int=?, /*** Default: -1 (until end of string) */
    re,
    string
  ) =>
  [ | `Full | `Partial | `Mismatch];

/** Marks */

module Mark: {
  /** Mark id */

  type t;

  /** Tell if a mark was matched. */

  let test: (Group.t, t) => bool;

  module Set: Set.S with type elt = t;

  /** Return all the mark matched. */

  let all: Group.t => Set.t;

  let equal: (t, t) => bool;
  let compare: (t, t) => int;
};

/** {2 High Level Operations} */;

type split_token = [
  | /** Text between delimiters */
    `Text(string)
  | /** Delimiter */
    `Delim(Group.t)
];

module Seq: {
  /** Same as {!all} but returns an iterator
        @since NEXT_RELEASE */

  let all:
    (
      ~pos: int=?, /*** Default: 0 */
      ~len: int=?,
      re,
      string
    ) =>
    Seq.t(Group.t);

  /** Same as {!matches}, but returns an iterator
        @since NEXT_RELEASE */

  let matches:
    (
      ~pos: int=?, /*** Default: 0 */
      ~len: int=?,
      re,
      string
    ) =>
    Seq.t(string);

  /** @since NEXT_RELEASE */

  let split:
    (
      ~pos: int=?, /*** Default: 0 */
      ~len: int=?,
      re,
      string
    ) =>
    Seq.t(string);

  /** @since NEXT_RELEASE */

  let split_full:
    (
      ~pos: int=?, /*** Default: 0 */
      ~len: int=?,
      re,
      string
    ) =>
    Seq.t(split_token);
};

/** Repeatedly calls {!exec} on the given string, starting at given position and
    length.*/

let all: (~pos: int=?, ~len: int=?, re, string) => list(Group.t);

type gen('a) = unit => option('a);

[@ocaml.deprecated "Use Seq.all"]
let all_gen: (~pos: int=?, ~len: int=?, re, string) => gen(Group.t);

/** Same as {!all}, but extracts the matched substring rather than returning
    the whole group. This basically iterates over matched strings */

let matches: (~pos: int=?, ~len: int=?, re, string) => list(string);

[@ocaml.deprecated "Use Seq.matches"]
let matches_gen: (~pos: int=?, ~len: int=?, re, string) => gen(string);

/** [split re s] splits [s] into chunks separated by [re]. It yields the chunks
    themselves, not the separator. For instance this can be used with a
    whitespace-matching re such as ["[\t ]+"]. */

let split: (~pos: int=?, ~len: int=?, re, string) => list(string);

[@ocaml.deprecated "Use Seq.split"]
let split_gen: (~pos: int=?, ~len: int=?, re, string) => gen(string);

/** [split re s] splits [s] into chunks separated by [re]. It yields the chunks
    along with the separators. For instance this can be used with a
    whitespace-matching re such as ["[\t ]+"]. */

let split_full: (~pos: int=?, ~len: int=?, re, string) => list(split_token);

[@ocaml.deprecated "Use Seq.split_full"]
let split_full_gen:
  (~pos: int=?, ~len: int=?, re, string) => gen(split_token);

/** [replace ~all re ~f s] iterates on [s], and replaces every occurrence
    of [re] with [f substring] where [substring] is the current match.
    If [all = false], then only the first occurrence of [re] is replaced. */

let replace:
  (
    ~pos: int=?, /*** Default: 0 */
    ~len: int=?,
    ~all: bool=?, /*** Default: true. Otherwise only replace first occurrence */
    re, /*** matched groups */
    ~f: Group.t => string, /*** how to replace */
    string
  ) => /*** string to replace in */
  string;

/** [replace_string ~all re ~by s] iterates on [s], and replaces every
    occurrence of [re] with [by]. If [all = false], then only the first
    occurrence of [re] is replaced. */

let replace_string:
  (
    ~pos: int=?, /*** Default: 0 */
    ~len: int=?,
    ~all: bool=?, /*** Default: true. Otherwise only replace first occurrence */
    re, /*** matched groups */
    ~by: string, /*** replacement string */
    string
  ) => /*** string to replace in */
  string;

/** {2 String expressions (literal match)} */;

let str: string => t;
let char: char => t;

/** {2 Basic operations on regular expressions} */;

/** Alternative.

    [alt []] is equivalent to {!empty}.

    By default, the leftmost match is preferred (see match semantics below).
*/

let alt: list(t) => t;

/** Sequence */

let seq: list(t) => t;

/** Match nothing */

let empty: t;

/** Empty word */

let epsilon: t;

/** 0 or more matches */

let rep: t => t;

/** 1 or more matches */

let rep1: t => t;

/** [repn re i j] matches [re] at least [i] times
    and at most [j] times, bounds included.
    [j = None] means no upper bound.
*/

let repn: (t, int, option(int)) => t;

/** 0 or 1 matches */

let opt: t => t;

/** {2 String, line, word}

    We define a word as a sequence of latin1 letters, digits and underscore.
*/;

/** Beginning of line */

let bol: t;

/** End of line */

let eol: t;

/** Beginning of word */

let bow: t;

/** End of word */

let eow: t;

/** Beginning of string. This differs from {!start} because it matches
    the beginning of the input string even when using [~pos] arguments:

    {[
      let b = execp (compile (seq [ bos; str "a" ])) "aa" ~pos:1 in
      assert (not b)
    ]}
*/

let bos: t;

/** End of string. This is different from {!stop} in the way described
    in {!bos}. */

let eos: t;

/** Last end of line or end of string */

let leol: t;

/** Initial position. This differs from {!bos} because it takes into
    account the [~pos] arguments:

    {[
      let b = execp (compile (seq [ start; str "a" ])) "aa" ~pos:1 in
      assert b
    ]}
*/

let start: t;

/** Final position. This is different from {!eos} in the way described
    in {!start}. */

let stop: t;

/** Word */

let word: t => t;

/** Not at a word boundary */

let not_boundary: t;

/** Only matches the whole string, i.e. [fun t -> seq [ eos; t; bos ]]. */

let whole_string: t => t;

/** {2 Match semantics}

   A regular expression frequently matches a string in multiple ways.  For
   instance [exec (compile (opt (str "a"))) "ab"] can match "" or "a". Match
   semantic can be modified with the functions below, allowing one to choose
   which of these is preferable.

   By default, the leftmost branch of alternations is preferred, and repetitions
   are greedy.

   Note that the existence of matches cannot be changed by specifying match
   semantics.  [seq [ bos; str "a"; non_greedy (opt (str "b")); eos ]] will
   match when applied to "ab". However if [seq [ bos; str "a"; non_greedy (opt
   (str "b")) ]] is applied to "ab", it will match "a" rather than "ab".

   Also note that multiple match semantics can conflict. In this case, the one
   executed earlier takes precedence. For instance, any match of [shortest (seq
   [ bos; group (rep (str "a")); group (rep (str "a")); eos ])] will always have
   an empty first group. Conversely, if we use [longest] instead of [shortest],
   the second group will always be empty.
*/;

/** Longest match semantics. That is, matches will match as many bytes as
    possible. If multiple choices match the maximum amount of bytes, the one
    respecting the inner match semantics is preferred. */

let longest: t => t;

/** Same as {!longest}, but matching the least number of bytes. */

let shortest: t => t;

/** First match semantics for alternations (not repetitions). That is, matches
    will prefer the leftmost branch of the alternation that matches the text. */

let first: t => t;

/** Greedy matches for repetitions ({!opt}, {!rep}, {!rep1}, {!repn}): they will
    match as many times as possible. */

let greedy: t => t;

/** Non-greedy matches for repetitions ({!opt}, {!rep}, {!rep1}, {!repn}): they
    will match as few times as possible. */

let non_greedy: t => t;

/** {2 Groups (or submatches)} */;

/** Delimit a group. The group is considered as matching if it is used at least
   once (it may be used multiple times if is nested inside {!rep} for
   instance). If it is used multiple times, the last match is what gets
   captured. */

let group: t => t;

/** Remove all groups */

let no_group: t => t;

/** When matching against [nest e], only the group matching in the
    last match of e will be considered as matching.

    For instance:
    {[
      let re = compile (rep1 (nest (alt [ group (str "a"); str "b" ]))) in
      let group = Re.exec re "ab" in
      assert (Group.get_opt group 1 = None);

      (* same thing but without [nest] *)
      let re = compile (rep1 (alt [ group (str "a"); str "b" ])) in
      let group = Re.exec re "ab" in
      assert (Group.get_opt group 1 = Some "a");
    ]}
*/

let nest: t => t;

/** Mark a regexp. the markid can then be used to know if this regexp was used. */

let mark: t => (Mark.t, t);

/** {2 Character sets} */;

/** Any character of the string */

let set: string => t;

/** Character ranges */

let rg: (char, char) => t;

/** Intersection of character sets */

let inter: list(t) => t;

/** Difference of character sets */

let diff: (t, t) => t;

/** Complement of union */

let compl: list(t) => t;

/** {2 Predefined character sets} */;

/** Any character */

let any: t;

/** Any character but a newline */

let notnl: t;

let alnum: t;
let wordc: t;
let alpha: t;
let ascii: t;
let blank: t;
let cntrl: t;
let digit: t;
let graph: t;
let lower: t;
let print: t;
let punct: t;
let space: t;
let upper: t;
let xdigit: t;

/** {2 Case modifiers} */;

/** Case sensitive matching. Note that this works on latin1, not ascii and not
    utf8. */

let case: t => t;

/** Case insensitive matching. Note that this works on latin1, not ascii and not
    utf8. */

let no_case: t => t;

/****/

/** {2 Internal debugging}  */;

let pp: (Format.formatter, t) => unit;

let pp_re: (Format.formatter, re) => unit;

/** Alias for {!pp_re}. Deprecated */

let print_re: (Format.formatter, re) => unit;

module View:
   {
    type outer;

    /** A view of the top-level of a regex. This type is unstable and may change */

    type t =
      | Set(Cset.t)
      | Sequence(list(outer))
      | Alternative(list(outer))
      | Repeat(outer, int, option(int))
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
      | Sem(Automata.sem, outer)
      | Sem_greedy(Automata.rep_kind, outer)
      | Group(outer)
      | No_group(outer)
      | Nest(outer)
      | Case(outer)
      | No_case(outer)
      | Intersection(list(outer))
      | Complement(list(outer))
      | Difference(outer, outer)
      | Pmark(Pmark.t, outer);

    let view: outer => t;
  } with
    type outer := t;

/** {2 Experimental functions} */;

/** [witness r] generates a string [s] such that [execp (compile r) s] is true.

    Be warned that this function is buggy because it ignores zero-width
    assertions like beginning of words. As a result it can generate incorrect
    results. */

let witness: t => string;

/** {2 Deprecated functions} */;

/** Alias for {!Group.t}. Deprecated */

[@ocaml.deprecated "Use Group.t"]
type substrings = Group.t;

/** Same as {!Group.get}. Deprecated */

[@ocaml.deprecated "Use Group.get"]
let get: (Group.t, int) => string;

/** Same as {!Group.offset}. Deprecated */

[@ocaml.deprecated "Use Group.offset"]
let get_ofs: (Group.t, int) => (int, int);

/** Same as {!Group.all}. Deprecated */

[@ocaml.deprecated "Use Group.all"]
let get_all: Group.t => array(string);

/** Same as {!Group.all_offset}. Deprecated */

[@ocaml.deprecated "Use Group.all_offset"]
let get_all_ofs: Group.t => array((int, int));

/** Same as {!Group.test}. Deprecated */

[@ocaml.deprecated "Use Group.test"]
let test: (Group.t, int) => bool;

/** Alias for {!Mark.t}. Deprecated */

[@ocaml.deprecated "Use Mark."]
type markid = Mark.t;

/** Same as {!Mark.test}. Deprecated */

[@ocaml.deprecated "Use Mark.test"]
let marked: (Group.t, Mark.t) => bool;

/** Same as {!Mark.all}. Deprecated */

[@ocaml.deprecated "Use Mark.all"]
let mark_set: Group.t => Mark.Set.t;
