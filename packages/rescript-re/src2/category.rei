/** Categories represent the various kinds of characters that can be tested
    by look-ahead and look-behind operations.

    This is more restricted than Cset, but faster.
*/;

type t;
let (++): (t, t) => t;
let from_char: char => t;

let dummy: t;
let inexistant: t;
let letter: t;
let not_letter: t;
let newline: t;
let lastnewline: t;
let search_boundary: t;
let to_int: t => int;
let equal: (t, t) => bool;
let compare: (t, t) => int;

let intersect: (t, t) => bool;

let pp: ( t) => unit;
