/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  linking exception.                                                 */
/*                                                                     */
/***********************************************************************/

/* Modified by Jerome.Vouillon@pps.jussieu.fr for integration in RE */

/* $Id: re_str.ml,v 1.3 2002/07/03 15:47:54 vouillon Exp $ */

module Re = Core;

type regexp = {
  re: Re.t,
  mtch: Lazy.t(Re.re),
  srch: Lazy.t(Re.re),
};

let compile_regexp = (s, c) => {
  let re = Emacs.re(~case=!c, s);
  {
    re,
    mtch: lazy(Re.compile(Re.seq([Re.start, re]))),
    srch: lazy(Re.compile(re)),
  };
};

let state = ref(None);

let string_match = (re, s, p) =>
  try(
    {
      state := Some(Re.exec(~pos=p, Lazy.force(re.mtch), s));
      true;
    }
  ) {
  | Not_found =>
    state := None;
    false;
  };

let string_partial_match = (re, s, p) =>
  switch (Re.exec_partial(~pos=p, Lazy.force(re.mtch), s)) {
  | `Full => string_match(re, s, p)
  | `Partial => true
  | `Mismatch => false
  };

let search_forward = (re, s, p) =>
  try({
    let res = Re.exec(~pos=p, Lazy.force(re.srch), s);
    state := Some(res);
    fst(Re.Group.offset(res, 0));
  }) {
  | Not_found =>
    state := None;
    raise(Not_found);
  };

let rec search_backward = (re, s, p) =>
  try({
    let res = Re.exec(~pos=p, Lazy.force(re.mtch), s);
    state := Some(res);
    p;
  }) {
  | Not_found =>
    state := None;
    if (p == 0) {
      raise(Not_found);
    } else {
      search_backward(re, s, p - 1);
    };
  };

let valid_group = n =>
  n >= 0
  && n < 10
  && (
    switch (state^) {
    | None => false
    | Some(m) => n < Re.Group.nb_groups(m)
    }
  );

let offset_group = i =>
  switch (state^) {
  | Some(m) => Re.Group.offset(m, i)
  | None => raise(Not_found)
  };

let group_len = i =>
  try({
    let (b, e) = offset_group(i);
    e - b;
  }) {
  | Not_found => 0
  };

let rec repl_length = (repl, p, q, len) =>
  if (p < len) {
    if (repl.[p] != '\\') {
      repl_length(repl, p + 1, q + 1, len);
    } else {
      let p = p + 1;
      if (p == len) {
        failwith("Str.replace: illegal backslash sequence");
      };
      let q =
        switch (repl.[p]) {
        | '\\' => q + 1
        | '0' .. '9' as c => q + group_len(Char.code(c) - Char.code('0'))
        | _ => q + 2
        };
      repl_length(repl, p + 1, q, len);
    };
  } else {
    q;
  };

let rec replace = (orig, repl, p, res, q, len) =>
  if (p < len) {
    let c = repl.[p];
    if (c != '\\') {
      Bytes.set(res, q, c);
      replace(orig, repl, p + 1, res, q + 1, len);
    } else {
      switch (repl.[p + 1]) {
      | '\\' =>
        Bytes.set(res, q, '\\');
        replace(orig, repl, p + 2, res, q + 1, len);
      | '0' .. '9' as c =>
        let d =
          try({
            let (b, e) = offset_group(Char.code(c) - Char.code('0'));
            let d = e - b;
            if (d > 0) {
              String.blit(orig, b, res, q, d);
            };
            d;
          }) {
          | Not_found => 0
          };

        replace(orig, repl, p + 2, res, q + d, len);
      | c =>
        Bytes.set(res, q, '\\');
        Bytes.set(res, q + 1, c);
        replace(orig, repl, p + 2, res, q + 2, len);
      };
    };
  };

let replacement_text = (repl, orig) => {
  let len = String.length(repl);
  let res = Bytes.create(repl_length(repl, 0, 0, len));
  replace(orig, repl, 0, res, 0, String.length(repl));
  Bytes.unsafe_to_string(res);
};

let quote = s => {
  let len = String.length(s);
  let buf = Buffer.create(2 * len);
  for (i in 0 to len - 1) {
    switch (s.[i]) {
    | ('[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$') as c =>
      Buffer.add_char(buf, '\\');
      Buffer.add_char(buf, c);
    | c => Buffer.add_char(buf, c)
    };
  };
  Buffer.contents(buf);
};

let string_before = (s, n) => String.sub(s, 0, n);

let string_after = (s, n) => String.sub(s, n, String.length(s) - n);

let first_chars = (s, n) => String.sub(s, 0, n);

let last_chars = (s, n) => String.sub(s, String.length(s) - n, n);

let regexp = e => compile_regexp(e, false);

let regexp_case_fold = e => compile_regexp(e, true);

let regexp_string = s => compile_regexp(quote(s), false);

let regexp_string_case_fold = s => compile_regexp(quote(s), true);

let group_beginning = n => {
  if (!valid_group(n)) {
    invalid_arg("Str.group_beginning");
  };
  let pos = fst(offset_group(n));
  if (pos == (-1)) {
    raise(Not_found);
  } else {
    pos;
  };
};

let group_end = n => {
  if (!valid_group(n)) {
    invalid_arg("Str.group_end");
  };
  let pos = snd(offset_group(n));
  if (pos == (-1)) {
    raise(Not_found);
  } else {
    pos;
  };
};

let matched_group = (n, txt) => {
  let (b, e) = offset_group(n);
  String.sub(txt, b, e - b);
};

let replace_matched = (repl, matched) => replacement_text(repl, matched);

let match_beginning = () => group_beginning(0)
and match_end = () => group_end(0)
and matched_string = txt => matched_group(0, txt);

let substitute_first = (expr, repl_fun, text) =>
  try({
    let pos = search_forward(expr, text, 0);
    String.concat(
      "",
      [
        string_before(text, pos),
        repl_fun(text),
        string_after(text, match_end()),
      ],
    );
  }) {
  | Not_found => text
  };

let global_substitute = (expr, repl_fun, text) => {
  let rec replace = (accu, start, last_was_empty) => {
    let startpos =
      if (last_was_empty) {
        start + 1;
      } else {
        start;
      };
    if (startpos > String.length(text)) {
      [string_after(text, start), ...accu];
    } else {
      switch (search_forward(expr, text, startpos)) {
      | pos =>
        let end_pos = match_end();
        let repl_text = repl_fun(text);
        replace(
          [repl_text, String.sub(text, start, pos - start), ...accu],
          end_pos,
          end_pos == pos,
        );
      | exception Not_found => [string_after(text, start), ...accu]
      };
    };
  };

  String.concat("", List.rev(replace([], 0, false)));
};

let global_replace = (expr, repl, text) =>
  global_substitute(expr, replacement_text(repl), text)
and replace_first = (expr, repl, text) =>
  substitute_first(expr, replacement_text(repl), text);

let search_forward_progress = (re, s, p) => {
  let pos = search_forward(re, s, p);
  if (match_end() > p) {
    pos;
  } else if (p < String.length(s)) {
    search_forward(re, s, p + 1);
  } else {
    raise(Not_found);
  };
};

let bounded_split = (expr, text, num) => {
  let start =
    if (string_match(expr, text, 0)) {
      match_end();
    } else {
      0;
    };
  let rec split = (accu, start, n) =>
    if (start >= String.length(text)) {
      accu;
    } else if (n == 1) {
      [string_after(text, start), ...accu];
    } else {
      try({
        let pos = search_forward_progress(expr, text, start);
        split(
          [String.sub(text, start, pos - start), ...accu],
          match_end(),
          n - 1,
        );
      }) {
      | Not_found => [string_after(text, start), ...accu]
      };
    };
  List.rev(split([], start, num));
};

let split = (expr, text) => bounded_split(expr, text, 0);

let bounded_split_delim = (expr, text, num) => {
  let rec split = (accu, start, n) =>
    if (start > String.length(text)) {
      accu;
    } else if (n == 1) {
      [string_after(text, start), ...accu];
    } else {
      try({
        let pos = search_forward_progress(expr, text, start);
        split(
          [String.sub(text, start, pos - start), ...accu],
          match_end(),
          n - 1,
        );
      }) {
      | Not_found => [string_after(text, start), ...accu]
      };
    };
  if (text == "") {
    [];
  } else {
    List.rev(split([], 0, num));
  };
};

let split_delim = (expr, text) => bounded_split_delim(expr, text, 0);

type split_result =
  | Text(string)
  | Delim(string);

let bounded_full_split = (expr, text, num) => {
  let rec split = (accu, start, n) =>
    if (start >= String.length(text)) {
      accu;
    } else if (n == 1) {
      [Text(string_after(text, start)), ...accu];
    } else {
      try({
        let pos = search_forward_progress(expr, text, start);
        let s = matched_string(text);
        if (pos > start) {
          split(
            [
              Delim(s),
              Text(String.sub(text, start, pos - start)),
              ...accu,
            ],
            match_end(),
            n - 1,
          );
        } else {
          split([Delim(s), ...accu], match_end(), n - 1);
        };
      }) {
      | Not_found => [Text(string_after(text, start)), ...accu]
      };
    };
  List.rev(split([], 0, num));
};

let full_split = (expr, text) => bounded_full_split(expr, text, 0);
