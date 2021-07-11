module Re = Core;

type regexp = Re.re;

type flag = [ | `CASELESS | `MULTILINE | `ANCHORED];

type split_result =
  | Text(string)
  | Delim(string)
  | Group(int, string)
  | NoGroup;

type groups = Core.Group.t;

let re = (~flags=[], pat) => {
  let opts =
    List.map(
      fun
      | `CASELESS => `Caseless
      | `MULTILINE => `Multiline
      | `ANCHORED => `Anchored,
      flags,
    );
  Perl.re(~opts, pat);
};

let regexp = (~flags=?, pat) => Re.compile(re(~flags?, pat));

let extract = (~rex, s) => Re.Group.all(Re.exec(rex, s));

let exec = (~rex, ~pos=?, s) => Re.exec(rex, ~pos?, s);

let get_substring = (s, i) => Re.Group.get(s, i);

let get_substring_ofs = (s, i) => Re.Group.offset(s, i);

let pmatch = (~rex, s) => Re.execp(rex, s);

let substitute = (~rex, ~subst, str) => {
  let b = Buffer.create(1024);
  let rec loop = pos =>
    if (pos >= String.length(str)) {
      Buffer.contents(b);
    } else if (Re.execp(~pos, rex, str)) {
      let ss = Re.exec(~pos, rex, str);
      let (start, fin) = Re.Group.offset(ss, 0);
      let pat = Re.Group.get(ss, 0);
      Buffer.add_substring(b, str, pos, start - pos);
      Buffer.add_string(b, subst(pat));
      loop(fin);
    } else {
      Buffer.add_substring(b, str, pos, String.length(str) - pos);
      loop(String.length(str));
    };

  loop(0);
};

let split = (~rex, str) => {
  let rec loop = (accu, pos) =>
    if (pos >= String.length(str)) {
      List.rev(accu);
    } else if (Re.execp(~pos, rex, str)) {
      let ss = Re.exec(~pos, rex, str);
      let (start, fin) = Re.Group.offset(ss, 0);
      let s = String.sub(str, pos, start - pos);
      loop([s, ...accu], fin);
    } else {
      let s = String.sub(str, pos, String.length(str) - pos);
      loop([s, ...accu], String.length(str));
    };
  loop([], 0);
};

/* From PCRE */
let string_unsafe_sub = (s, ofs, len) => {
  let r = Bytes.create(len);
  Bytes.unsafe_blit(s, ofs, r, 0, len);
  Bytes.unsafe_to_string(r);
};

let quote = s => {
  let len = String.length(s);
  let buf = Bytes.create(len lsl 1);
  let pos = ref(0);
  for (i in 0 to len - 1) {
    switch (String.unsafe_get(s, i)) {
    | ('\\' | '^' | '$' | '.' | '[' | '|' | '(' | ')' | '?' | '*' | '+' | '{') as c =>
      Bytes.unsafe_set(buf, pos^, '\\');
      incr(pos);
      Bytes.unsafe_set(buf, pos^, c);
      incr(pos);
    | c =>
      Bytes.unsafe_set(buf, pos^, c);
      incr(pos);
    };
  };
  string_unsafe_sub(buf, 0, pos^);
};

let full_split = (~max=0, ~rex, s) =>
  if (String.length(s) == 0) {
    [];
  } else if (max == 1) {
    [Text(s)];
  } else {
    let results = Re.split_full(rex, s);
    let matches =
      List.map(
        fun
        | `Text(s) => [Text(s)]
        | `Delim(d) => {
            let matches = Re.Group.all_offset(d);
            let delim = Re.Group.get(d, 0);
            [
              Delim(delim),
              ...{
                   let l = ref([]);
                   for (i in 1 to Array.length(matches) - 1) {
                     l :=
                       [
                         if (matches[i] == ((-1), (-1))) {
                           NoGroup;
                         } else {
                             Group(i, Re.Group.get(d, i));
                         },
                         ...l^,
                       ];
                   };
                   List.rev(l^);
                 },
            ];
          },
        results,
      );
    List.concat(matches);
  };

type substrings = Group.t;
