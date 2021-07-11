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

/*
 What we could (should?) do:
 - a* ==> longest ((shortest (no_group a)* ), a | ())  (!!!)
 - abc understood as (ab)c
 - "((a?)|b)" against "ab" should not bind the first subpattern to anything

 Note that it should be possible to handle "(((ab)c)d)e" efficiently
 */
module Re = Core;

exception Parse_error;
exception Not_supported;

let parse = (newline, s) => {
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
  let get = () => {
    let r = s.[i^];
    incr(i);
    r;
  };
  let unget = () => decr(i);

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
      Re.rep(Re.nest(r));
    } else if (accept('+')) {
      Re.rep1(Re.nest(r));
    } else if (accept('?')) {
      Re.opt(r);
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
        Re.repn(Re.nest(r), i, j);
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
      if (newline) {Re.notnl} else {Re.any};
    } else if (accept('(')) {
      let r = regexp();
      if (!accept(')')) {
        raise(Parse_error);
      };
      Re.group(r);
    } else if (accept('^')) {
      if (newline) {Re.bol} else {Re.bos};
    } else if (accept('$')) {
      if (newline) {Re.eol} else {Re.eos};
    } else if (accept('[')) {
      if (accept('^')) {
        Re.diff(Re.compl(bracket([])), Re.char('\n'));
      } else {
        Re.alt(bracket([]));
      };
    } else if (accept('\\')) {
      if (eos()) {
        raise(Parse_error);
      };
      switch (get()) {
      | (
          '|' | '(' | ')' | '*' | '+' | '?' | '[' | '.' | '^' | '$' | '{' | '\\'
        ) as c =>
        Re.char(c)
      | _ => raise(Parse_error)
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
      let c = char();
      if (accept('-')) {
        if (accept(']')) {
          [Re.char(c), Re.char('-'), ...s];
        } else {
          let c' = char();
          bracket([Re.rg(c, c'), ...s]);
        };
      } else {
        bracket([Re.char(c), ...s]);
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
      } else if (accept(':')) {
        raise(
          Not_supported /*XXX*/
        );
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
        c;
      } else {
        c;
      };
    } else {
      c;
    };
  };

  let res = regexp();
  if (!eos()) {
    raise(Parse_error);
  };
  res;
};

type opt = [ | `ICase | `NoSub | `Newline];

let re = (~opts=[], s) => {
  let r = parse(List.memq(`Newline, opts), s);
  let r =
    if (List.memq(`ICase, opts)) {
      Re.no_case(r);
    } else {
      r;
    };
  let r =
    if (List.memq(`NoSub, opts)) {
      Re.no_group(r);
    } else {
      r;
    };
  r;
};

let compile = re => Re.compile(Re.longest(re));
let compile_pat = (~opts=[], s) => compile(re(~opts, s));
