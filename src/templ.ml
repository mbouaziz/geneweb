(* camlp4r ./pa_html.cmo *)
(* $Id: templ.ml,v 3.3 2001-02-15 06:22:17 ddr Exp $ *)

open Config;
open Util;

(* Parsing *)

type ast =
  [ Atext of string
  | Avar of string and list string
  | Atransl of bool and string and char
  | Awid_hei of string
  | Aif of ast_expr and list ast and list ast
  | Aforeach of string and list string and list ast ]
and ast_expr =
  [ Eor of ast_expr and ast_expr
  | Eand of ast_expr and ast_expr
  | Eequal of ast_expr and ast_expr
  | Enot of ast_expr
  | Estr of string
  | Evar of string and list string ]
;

type token =
  [ LPAREN | RPAREN | DOT | EQUAL | IDENT of string | STRING of string ]
;

value rec get_ident len =
  parser
  [ [: `('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c); strm :] ->
      get_ident (Buff.store len c) strm
  | [: :] -> Buff.get len ]
;

value rec get_string len =
  parser
  [ [: `'"' :] -> Buff.get len
  | [: `c; strm :] -> get_string (Buff.store len c) strm ]
;

value get_variable =
  let rec var_kont =
    parser
    [ [: `'.'; s = get_ident 0; sl = var_kont :] -> [s :: sl]
    | [: `';' :] -> []
    | [: :] -> [] ]
  in
  parser
  [ [: `'%' :] -> ("%", [])
  | [: v = get_ident 0; vl = var_kont :] -> (v, vl) ]
;

value rec get_token =
  parser
  [ [: `(' ' | '\t' | '\n' | '\r'); strm :] -> get_token strm
  | [: `'(' :] -> LPAREN
  | [: `')' :] -> RPAREN
  | [: `'.' :] -> DOT
  | [: `'=' :] -> EQUAL
  | [: `'"'; s = get_string 0 :] -> STRING s
  | [: s = get_ident 0 :] -> IDENT s ]
;

value buff = ref (String.create 80);

value buff_store len x =
  do if len >= String.length buff.val then
       buff.val := buff.val ^ String.create (String.length buff.val)
     else ();
     buff.val.[len] := x;
  return succ len
;

value buff_mstore len s =
  add_rec len 0 where rec add_rec len i =
    if i == String.length s then len
    else add_rec (buff_store len s.[i]) (succ i)
;

value buff_get len = String.sub buff.val 0 len;

value lexicon_word =
  let upper = parser [ [: `'*' :] -> True | [: :] -> False ] in
  let rec text len =
    parser
    [ [: `']' :] -> Buff.get len
    | [: `c; strm:] -> text (Buff.store len c) strm ]
  in
  parser [: upp = upper; s = text 0; `n :] -> Atransl upp s n
;

value parse_templ conf base strm =
  let parse_bool_expr () =
    let rec parse_1 =
      parser
      [ [: e = parse_2;
           e =
             parser
             [ [: `IDENT "or"; strm :] -> Eor e (parse_1 strm)
             | [: :] -> e ] :] -> e ]
    and parse_2 =
      parser
      [ [: e = parse_3;
           e =
             parser
             [ [: `IDENT "and"; strm :] -> Eand e (parse_2 strm)
             | [: :] -> e ] :] -> e ]
    and parse_3 =
      parser
      [ [: e = parse_4;
           e =
             parser
             [ [: `EQUAL; e2 = parse_4 :] -> Eequal e e2
             | [: :] -> e ] :] -> e ]
    and parse_4 =
      parser
      [ [: `LPAREN; e = parse_1;
           _ = parser [ [: `RPAREN :] -> () | [: :] -> () ] :] -> e
      | [: `IDENT "not"; e = parse_4 :] -> Enot e
      | [: `IDENT id; idl = ident_list :] -> Evar id idl
      | [: `STRING s :] -> Estr s
      | [: :] -> Evar "bad_variable" [] ]
    and ident_list =
      parser
      [ [: `DOT; `IDENT id; idl = ident_list :] -> [id :: idl]
      | [: :] -> [] ]
    in
    let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
    let r = parse_3 (Stream.from f) in
    do match strm with parser [ [: `';' :] -> () | [: :] -> () ]; return r
  in
  let rec parse_astl astl bol len end_list strm =
    match strm with parser
    [ [: `'%' :] ->
        let astl =
          if len = 0 then astl else [Atext (buff_get len) :: astl]
        in
        match get_variable strm with
        [ ("%", []) -> parse_astl [Atext "%" :: astl] False 0 end_list strm
        | (v, []) when List.mem v end_list -> (List.rev astl, v)
        | x ->
            let ast =
              match x with
              [ ("if", []) -> parse_if strm
              | ("foreach", []) -> parse_foreach strm
              | ("wid_hei", []) -> Awid_hei (get_ident 0 strm)
              | (v, vl) -> Avar v vl ]
            in
            parse_astl [ast :: astl] False 0 end_list strm ]
    | [: `'[' :] ->
        let astl =
          if len = 0 then astl else [Atext (buff_get len) :: astl]
        in
        let a = lexicon_word strm in
        parse_astl [a :: astl] False 0 end_list strm
    | [: `c :] ->
        let empty_c = c = ' ' || c = '\t' in
        let len = if empty_c && bol then len else buff_store len c in
        let bol = empty_c && bol || c = '\n' in
        parse_astl astl bol len end_list strm
    | [: :] ->
        let astl =
          if len = 0 then astl else [Atext (buff_get len) :: astl]
        in
        (List.rev astl, "") ]
  and parse_if strm =
    let e = parse_bool_expr () in
    let (al1, al2) =
      loop () where rec loop () =
        let (al1, tok) =
          parse_astl [] False 0 ["elseif"; "else"; "end"] strm
        in
        match tok with
        [ "elseif" ->
            let e2 = parse_bool_expr () in
            let (al2, al3) = loop () in
            (al1, [Aif e2 al2 al3])
        | "else" ->
            let (al2, _) = parse_astl [] False 0 ["end"] strm in
            (al1, al2)
        | _ -> (al1, []) ]
    in
    Aif e al1 al2
  and parse_foreach strm =
    let (v, vl) = get_variable strm in
    let (astl, _) = parse_astl [] False 0 ["end"] strm in
    Aforeach v vl astl
  in
  fst (parse_astl [] True 0 [] strm)
;

value open_templ conf dir name =
  let std_fname =
    List.fold_right Filename.concat [lang_dir.val; "etc"] (name ^ ".txt")
  in
  if dir = "" then try Some (open_in std_fname) with [ Sys_error _ -> None ]
  else
    let dir = Filename.basename dir in
    let fname = Filename.concat (Util.base_path ["etc"] dir) (name ^ ".txt") in
    try Some (open_in fname) with
    [ Sys_error _ ->
        if dir = conf.bname then
          try Some (open_in std_fname) with [ Sys_error _ -> None ]
        else None ]
;

value strip_newlines_after_variables =
  loop where rec loop =
    fun
    [ [Atext s :: astl] ->
        let s =
          if s.[0] = '\n' then String.sub s 1 (String.length s - 1) else s
        in
        [Atext s :: loop astl]
    | [Aif s alt ale :: astl] -> [Aif s (loop alt) (loop ale) :: loop astl]
    | [Aforeach s sl al :: astl] -> [Aforeach s sl (loop al) :: loop astl]
    | [(Avar _ _ as ast) :: astl] -> [ast :: loop astl]
    | [(Atransl _ _ _ | Awid_hei _ as ast1); ast2 :: astl] ->
        [ast1; ast2 :: loop astl]
    | [ast] -> [ast]
    | [] -> [] ]
;

value input conf base fname =
  let config_templ =
    try
      let s = List.assoc "template" conf.base_env in
      loop [] 0 0 where rec loop list i len =
        if i == String.length s then
          List.rev [Buff.get len :: list]
        else if s.[i] = ',' then
          loop [Buff.get len :: list] (i + 1) 0
        else
          loop list (i + 1) (Buff.store len s.[i])
    with
    [ Not_found -> [conf.bname; "*"] ]
  in
  let dir =
    match p_getenv conf.env "templ" with
    [ Some x when List.mem "*" config_templ -> x
    | Some x when List.mem x config_templ -> x
    | Some _ | None ->
        match config_templ with
        [ [] | ["*"] -> ""
        | [x :: _] -> x ] ]
  in
  let dir = Filename.basename dir in
  match open_templ conf dir fname with
  [ Some ic ->
      let astl = parse_templ conf base (Stream.of_channel ic) in
      let astl = strip_newlines_after_variables astl in
      do close_in ic; return astl
  | None ->
      let title _ = Wserver.wprint "Error" in
      do Util.header conf title;
         tag "ul" begin
           html_li conf;
           Wserver.wprint "Cannot access file \"%s/%s.txt\".\n"
             dir fname;
         end;
         Util.trailer conf;
      return raise Exit ]
;

(* Common evaluation functions *)

value print_body_prop conf base =
  let s =
    try " dir=" ^ Hashtbl.find conf.lexicon " !dir" with
    [ Not_found -> "" ]
  in
  let s = s ^ body_prop conf in
  Wserver.wprint "%s" s
;

value print_variable conf base =
  fun
  [ "action" -> Wserver.wprint "%s" conf.command
  | "base_header" -> include_hed_trl conf (Some base) ".hed"
  | "base_trailer" -> include_hed_trl conf (Some base) ".trl"
  | "body_prop" -> print_body_prop conf base
  | "hidden" -> Util.hidden_env conf
  | "highlight" -> Wserver.wprint "%s" conf.highlight
  | "nl" -> Wserver.wprint "\n"
  | s -> Wserver.wprint "%%%s;" s ]
;
