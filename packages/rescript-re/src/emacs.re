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

let parse = s => {
  let i = ref(0);
  let l = String.length(s);
  let eos = () => i^ == l;
  let test = c => !eos() && s.[i^] == c;
  let test2 = (c, c') => i^ + 1 < l && s.[i^] == c && s.[i^ + 1] == c';
  let accept = c => {
    let r = test(c);
    if (r) {
      incr(i);
    };
    r;
  };
  let accept2 = (c, c') => {
    let r = test2(c, c');
    if (r) {
      i := i^ + 2;
    };
    r;
  };
  let get = () => {
    let r = s.[i^];
    incr(i);
    r;
  };

  let rec regexp = () => regexp'(branch())
  and regexp' = left =>
    if (accept2('\\', '|')) {
      regexp'(Re.alt([left, branch()]));
    } else {
      left;
    }
  and branch = () => branch'([])
  and branch' = left =>
    if (eos() || test2('\\', '|') || test2('\\', ')')) {
      Re.seq(List.rev(left));
    } else {
      branch'([piece(), ...left]);
    }
  and piece = () => {
    let r = atom();
    if (accept('*')) {
      Re.rep(r);
    } else if (accept('+')) {
      Re.rep1(r);
    } else if (accept('?')) {
      Re.opt(r);
    } else {
      r;
    };
  }
  and atom = () =>
    if (accept('.')) {
      Re.notnl;
    } else if (accept('^')) {
      Re.bol;
    } else if (accept('$')) {
      Re.eol;
    } else if (accept('[')) {
      if (accept('^')) {
        Re.compl(bracket([]));
      } else {
        Re.alt(bracket([]));
      };
    } else if (accept('\\')) {
      if (accept('(')) {
        let r = regexp();
        if (!accept2('\\', ')')) {
          raise(Parse_error);
        };
        Re.group(r);
      } else if (accept('`')) {
        Re.bos;
      } else if (accept('\'')) {
        Re.eos;
      } else if (accept('=')) {
        Re.start;
      } else if (accept('b')) {
        Re.alt([Re.bow, Re.eow]);
      } else if (accept('B')) {
        Re.not_boundary;
      } else if (accept('<')) {
        Re.bow;
      } else if (accept('>')) {
        Re.eow;
      } else if (accept('w')) {
        Re.alt([Re.alnum, Re.char('_')]);
      } else if (accept('W')) {
        Re.compl([Re.alnum, Re.char('_')]);
      } else {
        if (eos()) {
          raise(Parse_error);
        };
        switch (get()) {
        | ('*' | '+' | '?' | '[' | ']' | '.' | '^' | '$' | '\\') as c =>
          Re.char(c)
        | '0' .. '9' => raise(Not_supported)
        | _ => raise(Parse_error)
        };
      };
    } else {
      if (eos()) {
        raise(Parse_error);
      };
      switch (get()) {
      | '*'
      | '+'
      | '?' => raise(Parse_error)
      | c => Re.char(c)
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
    get();
  };

  let res = regexp();
  if (!eos()) {
    raise(Parse_error);
  };
  res;
};

let re = (~case=true, s) => {
  let r = parse(s);
  if (case) {
    r;
  } else {
    Re.no_case(r);
  };
};

let compile = Re.compile;
let compile_pat = (~case=true, s) => compile(re(~case, s));
