
open Ocamlbuild_plugin

module Ocaml_compiler = Ocamlbuild_pack.Ocaml_compiler

let split_string s =
  let s = String.trim s in
  if s = "" then
    []
  else
    let rec loop cur wend l =
      if cur < 0 then
        String.sub s 0 (wend + 1) :: l
      else if s.[cur] = ' ' then
        if wend > cur then
          loop (cur - 1) (cur - 1) (String.sub s (cur + 1) (wend - cur) :: l)
        else
          loop (cur - 1) (cur - 1) l
      else
        loop (cur - 1) wend l in
    let last = String.length s - 1 in
    loop last last []

module Config = struct
  let camlp5 = "/home/mehdi/.opam/4.03.0/bin/camlp5"
  let camlp5o = "/home/mehdi/.opam/4.03.0/bin/camlp5o"
  let camlp5r = "/home/mehdi/.opam/4.03.0/bin/camlp5r"

  let camlp5_comm = "../tools/camlp5_comm.sh"
  let camlp5_flags = "-DUNIX"
  let camlp5d = "/home/mehdi/.opam/4.03.0/lib/camlp5"
end

module Debug = struct
  let print_tags f =
    Format.fprintf Format.std_formatter
      "@[<hv 2>Tags for file %s:@ %a@]@." f
      Tags.print (tags_of_pathname f)

  let print_include_dirs_of s =
    let res = Pathname.include_dirs_of s in
    Format.fprintf Format.std_formatter "@[<hv 2>include_dirs_of %S ->@ %a@]@." s (List.print Pathname.print) res
end

let ext_obj = !Options.ext_obj
let ext_dll = !Options.ext_dll
let x_o = "%"-.-ext_obj

let build_or_fail ~env ~build files =
  List.iter Outcome.ignore_good (build (List.map (fun x -> [env x]) files))

let get_pp filename =
  let pp = with_input_file filename (fun ic ->
      let first_line = String.trim (input_line ic) in
      if String.is_prefix "(*" first_line && String.is_suffix first_line "*)" then
        match String.index first_line ' ' with
        | exception Not_found -> None
        | i -> let first_line = String.trim (String.after first_line i) in
          match String.index first_line ' ' with
          | exception Not_found -> None
          | i -> let first_word = String.first_chars first_line i in
            let j = String.rindex first_line ' ' in
            Some (first_word, String.sub first_line i (j - i))
      else None
    ) in
  match pp with
  | Some ("nocamlp5", _) -> None
  | Some (("camlp5" | "camlp5o" | "camlp5r"), _) -> pp
  | _ -> Some (Config.camlp5r, "")

let camlp5_comm dep prod env build =
  (*Cmd(S[A Config.camlp5_comm; A Config.camlp5_flags; A (env dep); A "-o"; A (env prod)])*)
  let dep_file = env dep in
  match get_pp dep_file with
  | None -> failwith "camlp5_comm should not be called on nocamlp5 files"
  | Some (pp_name, pp_args) ->
    let cur_dir = Filename.dirname dep_file in
    let pp_deps = List.filter (String.is_prefix ".") (split_string pp_args)
                  |> List.map (fun s -> Pathname.normalize (cur_dir / s)) in
    build_or_fail ~env ~build pp_deps;
    Cmd(S[P pp_name; Sh pp_args; A "pa_macro.cmo"; S (List.map (fun opt -> A opt) !Options.ocaml_ppflags); P dep_file; A "-o"; P (env prod)]) (*TODO: incdirs *)

let with_pp (orig, orig_deps) (pp, pp_deps) f env build =
  let orig_file = env orig in
  let x, to_build = match get_pp orig_file with
    | None -> orig, orig_deps
    | Some _ ->
      let pp_file = env pp in
      tag_file pp_file (Tags.elements (tags_of_pathname orig_file));
      Debug.print_tags orig_file;
      Debug.print_tags pp_file;
      Debug.print_include_dirs_of (Pathname.dirname orig_file);
      Debug.print_include_dirs_of (Pathname.dirname pp_file);
      pp, pp::pp_deps in
  build_or_fail ~env ~build to_build;
  f x env build

let () =
  dispatch begin function
  | After_rules ->
    rule "preprocess: ml -> pp5.ml"
      ~dep: "%.ml"
      ~prod: "%.pp5.ml"
      (camlp5_comm "%.ml" "%.pp5.ml");

    rule "preprocess: mli -> pp5.mli"
      ~prod:"%.pp5.mli"
      ~dep:"%.mli"
      (camlp5_comm "%.mli" "%.pp5.mli");

    rule "ocaml with pp: mli -> cmi"
      ~insert:(`before "ocaml: mli -> cmi")
      ~prod:"%.cmi"
      ~dep:"%.mli"
      (with_pp ("%.mli", ["%.mli.depends"]) ("%.pp5.mli", ["%.pp5.mli.depends"])
         (fun mli -> Ocaml_compiler.compile_ocaml_interf mli "%.cmi"));

    rule "ocaml with_pp: ml -> cmo & cmi"
      ~insert:(`before "ocaml: ml -> cmo & cmi")
      ~prods:["%.cmo"; "%.cmi"]
      ~dep:"%.ml"
      (with_pp ("%.ml", ["%.ml.depends"]) ("%.pp5.ml", ["%.pp5.ml.depends"])
         (fun ml -> Ocaml_compiler.byte_compile_ocaml_implem ml "%.cmo"));

    rule "ocaml with_pp: ml & cmi -> cmo"
      ~insert:(`before "ocaml: ml & cmi -> cmo")
      ~prod:"%.cmo"
      ~deps:["%.ml"; "%.mli"; "%.cmi"]
      (with_pp ("%.ml", ["%.ml.depends"]) ("%.pp5.ml", ["%.pp5.ml.depends"])
         (fun ml -> Ocaml_compiler.byte_compile_ocaml_implem ml "%.cmo"));

    rule "ocaml with pp: ml & cmi -> cmx & o"
      ~insert:(`before "ocaml: ml & cmi -> cmx & o")
      ~prods:["%.cmx"; x_o]
      ~deps:["%.ml"; "%.cmi"]
      (with_pp ("%.ml", ["%.ml.depends"]) ("%.pp5.ml", ["%.pp5.ml.depends"])
         (fun ml -> Ocaml_compiler.native_compile_ocaml_implem ml));



    (* Internal libs *)
    ocaml_lib "dag2html";
    ocaml_lib "wserver";

    (* External libs *)
    (* ocaml_lib ~extern:true ~dir:"+unix" "unix"; *)
    (* ocaml_lib ~extern:true ~dir:Findlib.((query "camlp5").location) "camlp5"; *)
    (* Pathname.define_context "src" [Findlib.((query "camlp5").location)];
    Debug.print_include_dirs_of "src"; *)
    (* flag ["ocaml";"ocamldep";"use_camlp5"] (S [A "-I"; P Findlib.((query "camlp5").location)]); *)

  | _ -> ()
  end
