type regexp = Core.re;

type flag = [ | `CASELESS | `MULTILINE | `ANCHORED];

type groups = Core.Group.t;

/** Result of a {!Pcre.full_split} */

type split_result =
  | /** Text part of splitted string */
    Text(string)
  | /** Delimiter part of splitted string */
    Delim(string)
  | /** Subgroup of matched delimiter (subgroup_nr, subgroup_str) */
    Group(
      int,
      string,
    )
  | /** Unmatched subgroup */
    NoGroup;

/** [re ~flags s] creates the regexp [s] using the pcre syntax. */

let re: (~flags: list(flag)=?, string) => Core.t;

/** [re ~flags s] compiles the regexp [s] using the pcre syntax. */

let regexp: (~flags: list(flag)=?, string) => regexp;

/** [extract ~rex s] executes [rex] on [s] and returns the matching groups. */

let extract: (~rex: regexp, string) => array(string);

/** Equivalent to {!Core.exec}. */

let exec: (~rex: regexp, ~pos: int=?, string) => groups;

/** Equivalent to {!Core.Group.get}. */

let get_substring: (groups, int) => string;

/** Equivalent to {!Core.Group.offset}. */

let get_substring_ofs: (groups, int) => (int, int);

/** Equivalent to {!Core.execp}. */

let pmatch: (~rex: regexp, string) => bool;

let substitute: (~rex: Core.re, ~subst: string => string, string) => string;

let full_split: (~max: int=?, ~rex: regexp, string) => list(split_result);

let split: (~rex: regexp, string) => list(string);

let quote: string => string;

/** {2 Deprecated} */;

type substrings = Group.t;
