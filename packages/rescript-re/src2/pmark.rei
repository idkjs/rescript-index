type t = pri int;
let equal: (t, t) => bool;
let compare: (t, t) => int;
let gen: unit => t;
let pp: (int, t) => unit;

module Set: Set.S with type elt = t;
