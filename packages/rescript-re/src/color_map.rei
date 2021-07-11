/* Color maps exists to provide an optimization for the regex engine. The fact
      that some characters are entirely equivalent for some regexes means that we
      can use them interchangeably.

      A color map assigns a color to every character in our character set. Any two
      characters with the same color will be treated equivalently by the automaton.
   */
type t;

let make: unit => t;

let flatten: t => (bytes, bytes, int);

let split: (Cset.t, t) => unit;
