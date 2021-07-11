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

module Re = Core;

exception Parse_error;
exception Not_supported;

let posix_class_of_string =
  fun
  | "alpha" => Re.alpha
  | "alnum" => Re.alnum
  | "ascii" => Re.ascii
  | "blank" => Re.blank
  | "cntrl" => Re.cntrl
  | "digit" => Re.digit
  | "lower" => Re.lower
  | "print" => Re.print
  | "space" => Re.space
  | "upper" => Re.upper
  | "word" => Re.wordc
  | "punct" => Re.punct
  | "graph" => Re.graph
  | "xdigit" => Re.xdigit
  | class_ => invalid_arg("Invalid pcre class: " ++ class_);

let posix_class_strings = [
  "alpha",
  "alnum",
  "ascii",
  "blank",
  "cntrl",
  "digit",
  "lower",
  "print",
  "space",
  "upper",
  "word",
  "punct",
  "graph",
  "xdigit",
];

let parse = (multiline, dollar_endonly, dotall, ungreedy, s) => {
  let i = ref(0);
  let l = String.length(s);
  let eos = () => i^ == l;
  let test = c => !eos() && s.[i^] == c;
  let accept = c => {
    let r = test(c);
    if (r) {
      incr(i);
    };
    r;
  };
  let accept_s = s' => {
    let len = String.length(s');
    try(
      {
        for (j in 0 to len - 1) {
          try(
            if (s'.[j] != s.[i^ + j]) {
              raise(Exit);
            }
          ) {
          | _ => raise(Exit)
          };
        };
        i := i^ + len;
        true;
      }
    ) {
    | Exit => false
    };
  };
  let get = () => {
    let r = s.[i^];
    incr(i);
    r;
  };
  let unget = () => decr(i);
  let greedy_mod = r => {
    let gr = accept('?');
    let gr =
      if (ungreedy) {
        !gr;
      } else {
        gr;
      };
    if (gr) {
      Re.non_greedy(r);
    } else {
      Re.greedy(r);
    };
  };

  let rec regexp = () => regexp'(branch())
  and regexp' = left =>
    if (accept('|')) {
      regexp'(Re.alt([left, branch()]));
    } else {
      left;
    }
  and branch = () => branch'([])
  and branch' = left =>
    if (eos() || test('|') || test(')')) {
      Re.seq(List.rev(left));
    } else {
      branch'([piece(), ...left]);
    }
  and piece = () => {
    let r = atom();
    if (accept('*')) {
      greedy_mod(Re.rep(r));
    } else if (accept('+')) {
      greedy_mod(Re.rep1(r));
    } else if (accept('?')) {
      greedy_mod(Re.opt(r));
    } else if (accept('{')) {
      switch (integer()) {
      | Some(i) =>
        let j =
          if (accept(',')) {
            integer();
          } else {
            Some(i);
          };
        if (!accept('}')) {
          raise(Parse_error);
        };
        switch (j) {
        | Some(j) when j < i => raise(Parse_error)
        | _ => ()
        };
        greedy_mod(Re.repn(r, i, j));
      | None =>
        unget();
        r;
      };
    } else {
      r;
    };
  }
  and atom = () =>
    if (accept('.')) {
      if (dotall) {Re.any} else {Re.notnl};
    } else if (accept('(')) {
      if (accept('?')) {
        if (accept(':')) {
          let r = regexp();
          if (!accept(')')) {
            raise(Parse_error);
          };
          r;
        } else if (accept('#')) {
          comment();
        } else {
          raise(Parse_error);
        };
      } else {
        let r = regexp();
        if (!accept(')')) {
          raise(Parse_error);
        };
        Re.group(r);
      };
    } else if (accept('^')) {
      if (multiline) {Re.bol} else {Re.bos};
    } else if (accept('$')) {
      if (multiline) {
        Re.eol;
      } else if (dollar_endonly) {
        Re.leol;
      } else {
        Re.eos;
      };
    } else if (accept('[')) {
      if (accept('^')) {
        Re.compl(bracket([]));
      } else {
        Re.alt(bracket([]));
      };
    } else if (accept('\\')) {
      /* XXX
            - Back-references
            - \cx (control-x), \e, \f, \n, \r, \t, \xhh, \ddd
         */
      if (eos()) {
        raise(Parse_error);
      };
      switch (get()) {
      | 'w' => Re.alt([Re.alnum, Re.char('_')])
      | 'W' => Re.compl([Re.alnum, Re.char('_')])
      | 's' => Re.space
      | 'S' => Re.compl([Re.space])
      | 'd' => Re.digit
      | 'D' => Re.compl([Re.digit])
      | 'b' => Re.alt([Re.bow, Re.eow])
      | 'B' => Re.not_boundary
      | 'A' => Re.bos
      | 'Z' => Re.leol
      | 'z' => Re.eos
      | 'G' => Re.start
      | 'a' .. 'z'
      | 'A' .. 'Z' => raise(Parse_error)
      | '0' .. '9' => raise(Not_supported)
      | c => Re.char(c)
      };
    } else {
      if (eos()) {
        raise(Parse_error);
      };
      switch (get()) {
      | '*'
      | '+'
      | '?'
      | '{'
      | '\\' => raise(Parse_error)
      | c => Re.char(c)
      };
    }
  and integer = () =>
    if (eos()) {
      None;
    } else {
      switch (get()) {
      | '0' .. '9' as d => integer'(Char.code(d) - Char.code('0'))
      | _ =>
        unget();
        None;
      };
    }
  and integer' = i =>
    if (eos()) {
      Some(i);
    } else {
      switch (get()) {
      | '0' .. '9' as d =>
        let i' = 10 * i + (Char.code(d) - Char.code('0'));
        if (i' < i) {
          raise(Parse_error);
        };
        integer'(i');
      | _ =>
        unget();
        Some(i);
      };
    }
  and bracket = s =>
    if (s != [] && accept(']')) {
      s;
    } else {
      switch (char()) {
      | `Char(c) =>
        if (accept('-')) {
          if (accept(']')) {
            [Re.char(c), Re.char('-'), ...s];
          } else {
            switch (char()) {
            | `Char(c') => bracket([Re.rg(c, c'), ...s])
            | `Set(st') => bracket([Re.char(c), Re.char('-'), st', ...s])
            };
          };
        } else {
          bracket([Re.char(c), ...s]);
        }
      | `Set(st) => bracket([st, ...s])
      };
    }
  and char = () => {
    if (eos()) {
      raise(Parse_error);
    };
    let c = get();
    if (c == '[') {
      if (accept('=')) {
        raise(Not_supported);
      };
      if (accept(':')) {
        let compl = accept('^');
        let cls =
          try(List.find(accept_s, posix_class_strings)) {
          | Not_found => raise(Parse_error)
          };
        if (!accept_s(":]")) {
          raise(Parse_error);
        };
        let re = {
          let posix_class = posix_class_of_string(cls);
          if (compl) {
            Re.compl([posix_class]);
          } else {
            posix_class;
          };
        };
        `Set(re);
      } else if (accept('.')) {
        if (eos()) {
          raise(Parse_error);
        };
        let c = get();
        if (!accept('.')) {
          raise(Not_supported);
        };
        if (!accept(']')) {
          raise(Parse_error);
        };
        `Char(c);
      } else {
        `Char(c);
      };
    } else if (c == '\\') {
      if (eos()) {
        raise(Parse_error);
      };
      let c = get();
      /* XXX
            \127, ...
         */
      switch (c) {
      | 'b' => `Char('\b')
      | 'n' => `Char('\n') /*XXX*/
      | 'r' => `Char('\r') /*XXX*/
      | 't' => `Char('\t') /*XXX*/
      | 'w' => `Set(Re.alt([Re.alnum, Re.char('_')]))
      | 'W' => `Set(Re.compl([Re.alnum, Re.char('_')]))
      | 's' => `Set(Re.space)
      | 'S' => `Set(Re.compl([Re.space]))
      | 'd' => `Set(Re.digit)
      | 'D' => `Set(Re.compl([Re.digit]))
      | 'a' .. 'z'
      | 'A' .. 'Z' => raise(Parse_error)
      | '0' .. '9' => raise(Not_supported)
      | _ => `Char(c)
      };
    } else {
      `Char(c);
    };
  }
  and comment = () => {
    if (eos()) {
      raise(Parse_error);
    };
    if (accept(')')) {
      Re.epsilon;
    } else {
      incr(i);
      comment();
    };
  };

  let res = regexp();
  if (!eos()) {
    raise(Parse_error);
  };
  res;
};

type opt = [
  | `Ungreedy
  | `Dotall
  | `Dollar_endonly
  | `Multiline
  | `Anchored
  | `Caseless
];

let re = (~opts=[], s) => {
  let r =
    parse(
      List.memq(`Multiline, opts),
      List.memq(`Dollar_endonly, opts),
      List.memq(`Dotall, opts),
      List.memq(`Ungreedy, opts),
      s,
    );

  let r =
    if (List.memq(`Anchored, opts)) {
      Re.seq([Re.start, r]);
    } else {
      r;
    };
  let r =
    if (List.memq(`Caseless, opts)) {
      Re.no_case(r);
    } else {
      r;
    };
  r;
};

let compile = Re.compile;
let compile_pat = (~opts=[], s) => compile(re(~opts, s));
