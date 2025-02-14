/* Result of a successful match. */
type t = {
  s:
    string,
    /* Input string. Matched strings are substrings of s */
  marks:
    Automata.mark_infos,
    /* Mapping from group indices to positions in gpos. group i has positions 2*i
       - 1, 2*i + 1 in gpos. If the group wasn't matched, then its corresponding
       values in marks will be -1,-1 */
  pmarks:
    Pmark.Set.t,
    /* Marks positions. i.e. those marks created with Re.marks */
  gpos:
    array(int),
    /* Group positions. Adjacent elements are (start, stop) of group match.
       indexed by the values in marks. So group i in an re would be the substring:

       start = t.gpos.(marks.(2*i)) - 1
       stop = t.gpos.(marks.(2*i + 1)) - 1 */
  gcount: int,
  /* Number of groups the regular expression contains. Matched or not */
};

/** Information about groups in a match. */;

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
