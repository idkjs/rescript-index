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

type enclosed =
  | Char(char)
  | Range(char, char);

type piece =
  | Exactly(char)
  | Any_of(list(enclosed))
  | Any_but(list(enclosed))
  | One
  | Many
  | ManyMany;

type t = list(piece);

let of_string = (~double_asterisk, s): t => {
  let i = ref(0);
  let l = String.length(s);
  let eos = () => i^ == l;
  let read = c => {
    let r = !eos() && s.[i^] == c;
    if (r) {
      incr(i);
    };
    r;
  };

  let char = () => {
    ignore(read('\\'): bool);
    if (eos()) {
      raise(Parse_error);
    };
    let r = s.[i^];
    incr(i);
    r;
  };

  let enclosed = (): list(enclosed) => {
    let rec loop = s =>
      /* This returns the list in reverse order, but order isn't important anyway */
      if (s != [] && read(']')) {
        s;
      } else {
        let c = char();
        if (!read('-')) {
          loop([Char(c), ...s]);
        } else if (read(']')) {
          [Char(c), Char('-'), ...s];
        } else {
          let c' = char();
          loop([ Range(c, c'), ...s]);
        };
      };

    loop([]);
  };

  let piece = () =>
    if (read('*')) {
      if (double_asterisk && read('*')) {
        ManyMany;
      } else {
        Many;
      };
    } else if (read('?')) {
      One;
    } else if (!read('[')) {
      Exactly(char());
    } else if (read('^') || read('!')) {
      Any_but(enclosed());
    } else {
      Any_of(enclosed());
    };

  let rec loop = pieces =>
    if (eos()) {
      List.rev(pieces);
    } else {
      loop([piece(), ...pieces]);
    };

  loop([]);
};

let mul = (l, l') =>
  List.flatten(List.map(s => List.map(s' => s ++ s', l'), l));

let explode = str => {
  let l = String.length(str);
  let rec expl = (inner, s, i, acc, beg) =>
    if (i >= l) {
      if (inner) {
        raise(Parse_error);
      };
      (mul(beg, [String.sub(str, s, i - s)]), i);
    } else {
      switch (str.[i]) {
      | '\\' => expl(inner, s, i + 2, acc, beg)
      | '{' =>
        let (t, i') = expl(true, i + 1, i + 1, [], [""]);
        expl(
          inner,
          i',
          i',
          acc,
          mul(beg, mul([String.sub(str, s, i - s)], t)),
        );
      | ',' when inner =>
        expl(
          inner,
          i + 1,
          i + 1,
          mul(beg, [String.sub(str, s, i - s)]) @ acc,
          [""],
        )
      | '}' when inner => (
          mul(beg, [String.sub(str, s, i - s)]) @ acc,
          i + 1,
        )
      | _ => expl(inner, s, i + 1, acc, beg)
      };
    };

  List.rev(fst(expl(false, 0, 0, [], [""])));
};

module State = {
  type t = {
    re_pieces: list(Re.t), /* last piece at head of list. */
    remaining: list(piece), /* last piece at tail of list. */
    am_at_start_of_pattern: bool, /* true at start of pattern */
    am_at_start_of_component: bool, /* true at start of pattern or immediately
                                              after '/' */
    pathname: bool,
    period: bool,
  };

  let create = (~period, ~pathname, remaining) => {
    re_pieces: [],
    am_at_start_of_pattern: true,
    am_at_start_of_component: true,
    pathname,
    period,
    remaining,
  };

  let explicit_period = t =>
    t.period
    && (t.am_at_start_of_pattern || t.am_at_start_of_component && t.pathname);

  let explicit_slash = t => t.pathname;

  let append = (~am_at_start_of_component=false, t, piece) => {
    ...t,
    re_pieces: [piece, ...t.re_pieces],
    am_at_start_of_pattern: false,
    am_at_start_of_component,
  };

  let to_re = t => Re.seq(List.rev(t.re_pieces));

  let next = t =>
    switch (t.remaining) {
    | [] => None
    | [piece, ...remaining] => Some((piece, {...t, remaining}))
    };
};

let one = (~explicit_slash, ~explicit_period) =>
  Re.compl(
    List.concat([
      if (explicit_slash) {
        [Re.char('/')];
      } else {
        [];
      },
      if (explicit_period) {
        [Re.char('.')];
      } else {
        [];
      },
    ]),
  );

let enclosed = enclosed =>
  switch (enclosed) {
  | Char(c) => Re.char(c)
  |  Range(low, high) => Re.rg(low, high)
  };

let enclosed_set = (~explicit_slash, ~explicit_period, kind, set) => {
  let set = List.map(enclosed, set);
  let enclosure =
    switch (kind) {
    | `Any_of => Re.alt(set)
    | `Any_but => Re.compl(set)
    };

  Re.inter([enclosure, one(~explicit_slash, ~explicit_period)]);
};

let exactly = (state, c) =>
  State.append(state, Re.char(c), ~am_at_start_of_component=c == '/');

let many_many = state => {
  let explicit_period = state.State.period && state.State.pathname;
  let first_explicit_period = State.explicit_period(state);
  let match_component = (~explicit_period) =>
    Re.seq([
      one(~explicit_slash=true, ~explicit_period),
      Re.rep(one(~explicit_slash=true, ~explicit_period=false)),
    ]);

  /* We must match components individually when [period] flag is set,
     making sure to not match ["foo/.bar"]. */
  State.append(
    state,
    Re.seq([
      Re.opt(match_component(~explicit_period=first_explicit_period)),
      Re.rep(
        Re.seq([Re.char('/'), Re.opt(match_component(~explicit_period))]),
      ),
    ]),
  );
};

let many = (state: State.t) => {
  let explicit_slash = State.explicit_slash(state);
  let explicit_period = State.explicit_period(state);
  /* Whether we must explicitly match period depends on the surrounding characters, but
        slashes are easy to explicit match. This conditional splits out some simple cases.
     */
  if (!explicit_period) {
    State.append(state, Re.rep(one(~explicit_slash, ~explicit_period)));
  } else if (!explicit_slash) {
    /* In this state, we explicitly match periods only at the very beginning */
    State.append(
      state,
      Re.opt(
        Re.seq([
          one(~explicit_slash=false, ~explicit_period),
          Re.rep(one(~explicit_slash=false, ~explicit_period=false)),
        ]),
      ),
    );
  } else {
    let not_empty =
      Re.seq([
        one(~explicit_slash=true, ~explicit_period=true),
        Re.rep(one(~explicit_slash=true, ~explicit_period=false)),
      ]);

    /* [maybe_empty] is the default translation of Many, except in some special cases.
     */
    let maybe_empty = Re.opt(not_empty);
    let enclosed_set = (state, kind, set) =>
      State.append(
        state,
        Re.alt([
          enclosed_set(
            kind,
            set,
            ~explicit_slash=true,
            ~explicit_period=true,
          ),
          Re.seq([
            not_empty,
            /* Since [not_empty] matched, subsequent dots are not leading. */
            enclosed_set(
              kind,
              set,
              ~explicit_slash=true,
              ~explicit_period=false,
            ),
          ]),
        ]),
      );

    let rec lookahead = state =>
      switch (State.next(state)) {
      | None => State.append(state, maybe_empty)
      /* glob ** === glob * . */
      | Some((Many, state)) => lookahead(state)
      | Some((Exactly(c), state)) =>
        let state =
          State.append(
            state,
            if (c == '.') {
              not_empty;
            } else {
              maybe_empty;
            },
          );

        exactly(state, c);
      /* glob *? === glob ?* */
      | Some((One, state)) => State.append(state, not_empty)
      | Some((Any_of(enclosed), state)) =>
        enclosed_set(state, `Any_of, enclosed)
      | Some((Any_but(enclosed), state)) =>
        enclosed_set(state, `Any_but, enclosed)
      /* * then ** === ** */
      | Some((ManyMany, state)) => many_many(state)
      };

    lookahead(state);
  };
};

let piece = (state, piece) => {
  let explicit_slash = State.explicit_slash(state);
  let explicit_period = State.explicit_period(state);
  switch (piece) {
  | One => State.append(state, one(~explicit_slash, ~explicit_period))
  | Many => many(state)
  | Any_of(enclosed) =>
    State.append(
      state,
      enclosed_set(`Any_of, ~explicit_slash, ~explicit_period, enclosed),
    )
  | Any_but(enclosed) =>
    State.append(
      state,
      enclosed_set(`Any_but, ~explicit_slash, ~explicit_period, enclosed),
    )
  | Exactly(c) => exactly(state, c)
  | ManyMany => many_many(state)
  };
};

let glob = (~pathname, ~period, glob) => {
  let rec loop = state =>
    switch (State.next(state)) {
    | None => State.to_re(state)
    | Some((p, state)) => loop(piece(state, p))
    };

  loop(State.create(~pathname, ~period, glob));
};

let glob =
    (
      ~anchored=false,
      ~pathname=true,
      ~period=true,
      ~expand_braces=false,
      ~double_asterisk=true,
      s,
    ) => {
  let to_re = s => {
    let re = glob(~pathname, ~period, of_string(~double_asterisk, s));
    if (anchored) {
      Re.whole_string(re);
    } else {
      re;
    };
  };

  if (expand_braces) {
    Re.alt(List.map(to_re, explode(s)));
  } else {
    to_re(s);
  };
};

let glob' = (~anchored=?, period, s) => glob(~anchored?, ~period, s);

let globx = (~anchored=?, s) => glob(~anchored?, ~expand_braces=true, s);

let globx' = (~anchored=?, period, s) =>
  glob(~anchored?, ~expand_braces=true, ~period, s);
