(* nocamlp5 *)
(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)


module M = Api_piqi
module Mext = Api_piqi_ext

open Config
open Gwdb
open Def
open Util
open Api_def
open Api_util



let string_start_with ini s =
  let rec loop i1 i2 =
    if i1 = String.length ini then true
    else if i2 = String.length s then
      if ini.[i1] = '_' then loop (i1 + 1) i2
      else false
    else if s.[i2] = ini.[i1] || s.[i2] = ' ' && ini.[i1] = '_' then
      loop (i1 + 1) (i2 + 1)
    else false
  in loop 0 0
;;

(* Algo de Knuth-Morris-Pratt *)
let init_next p =
  let m = String.length p in
  let next = Array.make m 0 in
  let i = ref 1 and j = ref 0 in
  while !i < m - 1 do
    if p.[!i] = p.[!j] then begin incr i; incr j; next.(!i) <- !j end
    else if !j = 0 then begin incr i; next.(!i) <- 0 end
    else j := next.(!j)
  done;
  next
;;

(* Algo de Knuth-Morris-Pratt *)
let kmp p s =
  let p = (Name.lower p) in
  let s = (Name.lower s) in
  (* Optimisation pour GeneWeb *)
  if s = "" || s = "?" then false
  else
    begin
      let next = init_next p and m = String.length p in
      let n = String.length s and i = ref 0 and j = ref 0 in
      while !j < m && !i < n do
        if s.[!i] = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.(!j)
      done;
      if !j >= m then true else false
    end
;;

let capitalize_if_not_utf8 s =
  if !Mutil.utf_8_db then s else String.capitalize s
;;

let new_name_key base s =
  let start_with2 s i p =
    i + String.length p <= String.length s &&
    String.sub s i (String.length p) = p
  in
  let parts =
    List.filter
      (fun p -> start_with2 (Name.lower s) 0 (Name.lower p ^ " "))
      (Gwdb.base_particles base)
  in
  let part =
    (function
      | [] -> ""
      | x :: _ -> x) parts
  in
  if part = "" then s
  else
    let i = String.length part in
    String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i
;;

let name_key_compatible base s =
  if !Mutil.utf_8_db then new_name_key base s else Mutil.name_key s
;;


let select_both_start_with conf base ini_n ini_p need_whole_list =
  let ini_n = name_key_compatible base ini_n in
  let name = persons_of_surname base in
  let search conv accu =
    let start_n = Mutil.tr '_' ' ' ini_n in
    let letter = conv (String.sub start_n 0 1) in
    match
      try Some (spi_first name letter) with
      Not_found -> None
    with
     | Some istr_n ->
        let rec loop istr_n list =
          let s = Mutil.nominative (sou base istr_n) in
          let k = name_key_compatible base s in
          if (String.sub k 0 1) = letter then
            if string_start_with (Name.lower ini_n) (Name.lower k) then
              let list =
                if s <> "?" then
                  let my_list = spi_find name istr_n in
                  let my_list =
                    List.fold_left
                      (fun l ip ->
                        if is_patched_person base ip then
                          let p = poi base ip in
                          let isn = get_surname p in
                          let isp = sou base (get_first_name p) in
                          if (eq_istr isn istr_n) &&
                            (string_start_with (Name.lower ini_p) (Name.lower isp))
                          then (ip :: l) else l
                        else
                          let p = poi base ip in
                          let isp = sou base (get_first_name p) in
                          if string_start_with (Name.lower ini_p) (Name.lower isp) then
                            (ip :: l)
                          else  l )
                      [] my_list
                  in
                  let my_list =
                    if conf.use_restrict then
                      List.fold_left
                        (fun l ip ->
                          if is_restricted conf base ip then l
                           else (ip :: l) )
                        [] my_list
                    else my_list
                  in
                  List.rev_append my_list list
                else list
              in
              match
                try Some (spi_next name istr_n need_whole_list) with
                  Not_found -> None
              with
               | Some (istr_n, _) -> loop istr_n list
               | None -> list
            else
              match
                try Some (spi_next name istr_n need_whole_list) with
                  Not_found -> None
              with
               | Some (istr_n, _) -> loop istr_n list
               | None -> list
          else list
        in loop istr_n accu
    | None -> accu
  in
  let list = search String.uppercase [] in
  search String.lowercase list
;;


let select_both_all conf base ini_n ini_p need_whole_list maiden_name =
  let find_sn p x = kmp x (sou base (get_surname p)) in
  let find_fn p x = kmp x (sou base (get_first_name p)) in
  let find_str s x = kmp x s in
  let ini_n = name_key_compatible base ini_n in
  let ini_n = code_varenv ini_n in
  let ini_n =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_n []
  in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = code_varenv ini_p in
  let ini_p =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_p []
  in
  let add_maiden p ini_p l =
    let rec loop ifam_l l =
      match ifam_l with
      | [] -> ()
      | ifam :: q ->
          let fam = foi base ifam in
          let ip = Gutil.spouse (get_key_index p) fam in
          let sp = poi base ip in
          if List.for_all (fun s -> find_fn sp s) ini_p then l := ip :: !l;
          loop q l
    in
    if get_sex p = Male then
      loop (Array.to_list (get_family p)) l
  in
  let add_maiden2 p ini_n ini_p l =
    (* On sépare les noms avec tirets ... *)
    let ini_n =
      List.fold_left
        (fun accu s -> (explode s '-') @ accu)
        [] ini_n
    in
    let rec loop ifam_l l =
      match ifam_l with
      | [] -> ()
      | ifam :: q ->
          let fam = foi base ifam in
          let ip = Gutil.spouse (get_key_index p) fam in
          let sp = poi base ip in
          let names =
            sou base (get_surname p) ^ " " ^ sou base (get_surname sp)
          in
          if List.for_all (fun s -> find_str names s) ini_n then
            add_maiden p ini_p l;
          loop q l
    in
    if get_sex p = Male then
      if List.exists (fun s -> find_sn p s) ini_n then
        loop (Array.to_list (get_family p)) l
  in
  let list = ref [] in
  for i = 0 to nb_of_persons base - 1 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    if List.for_all (fun s -> find_sn p s) ini_n then
      begin
        if List.for_all (fun s -> find_fn p s) ini_p then
          list := ip :: !list
        else if maiden_name then add_maiden p ini_p list
      end
    else
      (* On cherche une partie du nom de jeune fille dans les noms donnés. *)
      if maiden_name then add_maiden2 p ini_n ini_p list
  done;
  !list
;;


let select_start_with conf base is_surnames ini need_whole_list =
  let ini =
    if is_surnames then name_key_compatible base ini
    else ini
  in
  let name =
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  let search conv accu =
    let start_k = Mutil.tr '_' ' ' ini  in
    let letter = conv (String.sub start_k 0 1) in
    match
      try Some (spi_first name letter) with
      Not_found -> None
    with
     | Some istr ->
        let rec loop istr list =
          let s = (sou base istr) in
          let k = (name_key_compatible base s) in
          if (String.sub k 0 1) = letter then
            if string_start_with (Name.lower ini) (Name.lower k) then
              let list =
                if s <> "?" then
                  let my_list = spi_find name istr in
                  let my_list =
                    List.fold_left
                      (fun l ip ->
                        if is_patched_person base ip then
                          let p = poi base ip in
                          let isn =
                            if is_surnames then get_surname p
                            else get_first_name p
                          in
                          if eq_istr isn istr then (ip :: l) else l
                        else (ip :: l) )
                      [] my_list
                  in
                  let my_list =
                    if conf.use_restrict then
                      List.fold_left
                        (fun l ip ->
                          if is_restricted conf base ip then l
                          else (ip :: l) )
                        [] my_list
                    else my_list
                  in
                  List.rev_append my_list list
                else list
              in
              match
                try Some (spi_next name istr need_whole_list) with
                 Not_found -> None
              with
               | Some (istr, dlen) -> loop istr list
               | None -> list
            else
              match
                try Some (spi_next name istr need_whole_list) with
                 Not_found -> None
              with
               | Some (istr, dlen) -> loop istr list
               | None -> list
          else list
        in loop istr accu
     | None -> accu
  in
  let list = search String.uppercase [] in
  search String.lowercase list
;;


let select_all conf base is_surnames ini =
  let find p x =
    if is_surnames then kmp x (sou base (get_surname p))
    else kmp x (sou base (get_first_name p))
  in
  let ini =
    if is_surnames then name_key_compatible base ini
    else ini
  in
  let ini = code_varenv ini in
  let ini =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini []
  in
  let list = ref [] in
  for i = 0 to nb_of_persons base - 1 do
    if List.for_all (fun s -> find (poi base (Adef.iper_of_int i)) s) ini
    then list := (Adef.iper_of_int i) :: !list
  done;
  !list
;;


module Iper =
  struct
    type t = Adef.iper
    let compare i1 i2 =
      Pervasives.compare (Adef.int_of_iper i1) (Adef.int_of_iper i2)
  end

module IperSet = Set.Make(Iper) ;;

let print_list conf base filters list =
  let person_l =
    IperSet.elements
      (List.fold_left
         (fun accu p -> IperSet.add p accu)
         IperSet.empty list)
  in
  let person_l = List.rev_map (poi base) person_l in
  let person_l =
    if filters.nb_results then person_l
    else
      List.sort
        (fun p1 p2 ->
          let sn1 = Name.lower (p_surname base p1) in
          let sn2 = Name.lower (p_surname base p2) in
          let comp = Gutil.alphabetic_order sn1 sn2 in
          if comp = 0 then
            let fn1 = Name.lower (p_first_name base p1) in
            let fn2 = Name.lower (p_first_name base p2) in
            Gutil.alphabetic_order fn1 fn2
          else comp)
        person_l
  in
  let data = data_list_person conf base filters person_l in
  print_result conf data
;;

(*
   La différence entre la recherche approximative et lastname_or_surname est
   si on cherche un nom ET un prénom (dans les autres cas, on obtient les
   mêmes résultats.
   De se fait, on utilise list_n = select_all n, list_p = select_all p et on
   fait l'union des deux ce qui est beaucoup plus efficace.
*)
let print_search conf base =
  let search_params = get_params conf Mext.parse_search_params in
  let filters = get_filters conf in
  match
     (search_params.M.Search_params.lastname,
      search_params.M.Search_params.firstname)
  with
   | (Some n, Some fs) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower n = "" && Name.lower fs = "" then
          []
        else
          let maiden_name = search_params.M.Search_params.maiden_name in
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_both_start_with conf base n fs true
          | `approximative -> select_both_all conf base n fs true maiden_name
          | `lastname_or_firstname ->
               let list_n = select_all conf base true n in
               let list_p = select_all conf base false fs in
               List.rev_append list_n list_p
      in
      print_list conf base filters list
  | (Some n, None) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower n = "" then
          []
        else
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_start_with conf base true n true
          | `approximative -> select_all conf base true n
          | `lastname_or_firstname -> select_all conf base true n
      in
      print_list conf base filters list
  | (None, Some fs) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower fs = "" then
          []
        else
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_start_with conf base false fs true
          | `approximative -> select_all conf base false fs
          | `lastname_or_firstname -> select_all conf base false fs
      in
      print_list conf base filters list
  | (None, None) -> ()
;;



(**/**) (* Recherche utilisée pour l'auto-completion ou relier personne. *)


module StrSetAutoComplete =
  Set.Make
    (struct
      type t = string ;;
      let compare = compare ;;
     end)
;;

let rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i
;;

let rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i
;;

let string_incl_start_with x y =
  let rec loop j_ini =
    if j_ini = String.length y then false
    else
      let rec loop1 i j =
        if i = String.length x then true
        else if j = String.length y then
          if x.[i] = '_' then loop1 (i + 1) j
          else false
        else if y.[j] = x.[i] || y.[j] = ' ' && x.[i] = '_' then
          loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
  in
  loop 0
;;

let select_both_start_with_person conf base ini_n ini_p max_res =
  let find n x = string_start_with x n in
  let rec cut_at_space s acc =
    if String.contains s '+' then
      let index = String.index s '+' in
      let start = index + 1 in
      let len = String.length s - start in
      let ns = String.sub s start len in
      cut_at_space ns (decode_varenv (String.sub s 0 index) :: acc)
    else (decode_varenv s :: acc)
  in
  let ini_n = cut_at_space (code_varenv (Name.lower ini_n)) [] in
  let ini_p = cut_at_space (code_varenv (Name.lower ini_p)) [] in
  let rec loop i list nb_res =
    if (*nb_res < max_res &&*) i < nb_of_persons base then
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      let surnames =
        cut_at_space (code_varenv (Name.lower (sou base (get_surname p)))) []
      in
      let first_names =
        cut_at_space
          (code_varenv (Name.lower (sou base (get_first_name p)))) []
      in
      let start_surname =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) surnames)
          ini_n
      in
      let start_firstname =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) first_names)
          ini_p
      in
      if start_surname && start_firstname then
        loop (i + 1) (ip :: list) (nb_res + 1)
      else loop (i + 1) list nb_res
    else list
  in
  loop 0 [] 0
;;

let select_start_with_person conf base get_field max_res ini =
  let find n x = string_start_with x n in
  let rec cut_at_space s acc =
    if String.contains s '+' then
      let index = String.index s '+' in
      let start = index + 1 in
      let len = String.length s - start in
      let ns = String.sub s start len in
      cut_at_space ns (decode_varenv (String.sub s 0 index) :: acc)
    else (decode_varenv s :: acc)
  in
  let ini = cut_at_space (code_varenv (Name.lower ini)) [] in
  let rec loop i list nb_res =
    if (*nb_res < max_res &&*) i < nb_of_persons base then
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      let names =
        cut_at_space (code_varenv (Name.lower (sou base (get_field p)))) []
      in
      let start_name =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) names)
          ini
      in
      if start_name then loop (i + 1) (ip :: list) (nb_res + 1)
      else loop (i + 1) list nb_res
    else list
  in
  loop 0 [] 0
;;


let select_start_with_auto_complete conf base mode get_field max_res ini =
  let need_whole_list = true in
  let name =
    match mode with
    | `lastname -> persons_of_surname base
    | `firstname -> persons_of_first_name base
    | `place -> failwith "cannot use select_start_with_auto_complete"
    | `source -> failwith "cannot use select_start_with_auto_complete"
  in
  let string_set = ref StrSetAutoComplete.empty in
  let nb_res = ref 0 in
  (* Si la base est grosse > 100 000, on fait un vrai start_with. *)
  if Gwdb.nb_of_persons base > 100000 then
    begin
      (* majuscule *)
      let ini =
        match mode with
        | `lastname -> name_key_compatible base ini
        | `firstname -> ini
        | `place -> failwith "cannot use select_start_with_auto_complete"
        | `source -> failwith "cannot use select_start_with_auto_complete"
      in
      let start_k = Mutil.tr '_' ' ' ini in
      let letter = String.uppercase (String.sub start_k 0 1) in
      match
        try Some (spi_first name letter) with
          Not_found -> None
      with
      | Some istr ->
          let rec loop istr =
            let s = sou base istr in
            let k = name_key_compatible base s in
            if string_start_with (Name.lower ini) (Name.lower k) then
              begin
                string_set := StrSetAutoComplete.add s !string_set;
                incr nb_res;
                match
                  try Some (spi_next name istr need_whole_list) with
                   Not_found -> None
                with
                 | Some (istr, dlen) ->
                     if !nb_res < max_res && (String.sub k 0 1) = letter then loop istr
                     else ()
                 | None -> ()
              end
            else
              match
                try Some (spi_next name istr need_whole_list) with
                  Not_found -> None
              with
              | Some (istr, dlen) ->
                  if !nb_res < max_res && (String.sub k 0 1) = letter then loop istr
                  else ()
              | None -> ()
          in loop istr
      | None -> ();
      (* minuscule *)
      if !nb_res < max_res then
        let ini =
          match mode with
          | `lastname -> name_key_compatible base ini
          | `firstname -> ini
          | `place -> failwith "cannot use select_start_with_auto_complete"
          | `source -> failwith "cannot use select_start_with_auto_complete"
        in
        let start_k = Mutil.tr '_' ' ' ini in
        let letter = String.lowercase (String.sub start_k 0 1) in
        match
          try Some (spi_first name letter) with
            Not_found -> None
        with
        | Some istr ->
            let rec loop istr =
              let s = sou base istr in
              let k = name_key_compatible base s in
              if string_start_with (Name.lower ini) (Name.lower k) then
                begin
                  string_set := StrSetAutoComplete.add s !string_set;
                  incr nb_res;
                  match
                    try Some (spi_next name istr need_whole_list) with
                      Not_found -> None
                  with
                  | Some (istr, dlen) ->
                      if !nb_res < max_res && (String.sub k 0 1) = letter then loop istr
                      else ()
                  | None -> ()
                end
              else
                match
                  try Some (spi_next name istr need_whole_list) with
                    Not_found -> None
                with
                | Some (istr, dlen) ->
                    if !nb_res < max_res && (String.sub k 0 1) = letter then loop istr
                    else ()
                | None -> ()
            in loop istr
        | None -> ()
      else ()
    end
  else
    begin
      (* On commence à ? comme ça on fait MAJ et MIN. *)
      let start_k = Mutil.tr '_' ' ' "?" in
      let letter = String.uppercase (String.sub start_k 0 1) in
      match
        try Some (spi_first name letter) with
          Not_found -> None
      with
      | Some istr ->
          let rec loop istr list =
            let s = sou base istr in
            let k = name_key_compatible base s in
            if string_incl_start_with (Name.lower ini) (Name.lower k) then
              begin
                string_set := StrSetAutoComplete.add (sou base istr) !string_set;
                incr nb_res;
                match
                  try Some (spi_next name istr need_whole_list) with
                    Not_found -> None
                with
                | Some (istr, dlen) ->
                    if !nb_res < max_res then loop istr list
                    else ()
                | None -> ()
              end
            else
              match
                try Some (spi_next name istr need_whole_list) with
                  Not_found -> None
              with
              | Some (istr, dlen) ->
                  if !nb_res < max_res then loop istr list
                  else ()
              | None -> ()
          in loop istr []
      | None -> ()
    end;
  List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)
;;


let select_all_auto_complete conf base get_field max_res ini =
  let find p x = kmp x (sou base (get_field p)) in
  let ini = code_varenv ini in
  let ini =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini []
  in
  let string_set = ref StrSetAutoComplete.empty in
  let nb_res = ref 0 in
  for i = 0 to nb_of_persons base - 1 do
    if !nb_res < max_res then
      if List.for_all (fun s -> find (poi base (Adef.iper_of_int i)) s) ini
      then
        begin
        let p = poi base (Adef.iper_of_int i) in
        string_set := StrSetAutoComplete.add (sou base (get_field p)) !string_set;
        incr nb_res;
        end
  done;
  List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)
;;


let load_dico_lieu conf pl_mode =
  let fname =
  match pl_mode with
  | `town -> "dico_place_town_" ^ conf.lang ^ ".list"
  | `area_code -> "dico_place_area_code_" ^ conf.lang ^ ".list"
  | `county -> "dico_place_county_" ^ conf.lang ^ ".list"
  | `region -> "dico_place_region_" ^ conf.lang ^ ".list"
  | `country -> "dico_place_country_" ^ conf.lang ^ ".list"
  | _ -> ""
  in
  if fname = "" then []
  else
    let fname = Filename.concat "lang" fname in
    match
      try Some (Secure.open_in (Util.search_in_lang_path fname))
      with Sys_error _ -> None
    with
    | Some ic ->
        begin
          let list : (string list) = input_value ic in
          close_in ic;
          list
        end
    | None -> []
;;


let get_field mode =
  match mode with
  | `lastname -> get_surname
  | `firstname -> get_first_name
  | _ -> failwith "get_field"
;;


let search_auto_complete conf base mode place_mode max_res n =
  if mode = `place then
    begin
      let conf = {(conf) with env = ("data", "place") :: conf.env } in
      let list = UpdateData.get_all_data conf base in
      (* On fait un rev_map (tail-rec) parce que si le nombre de *)
      (* données est trop important, on casser la pile d'appels. *)
      let list = List.rev_map (fun (istr, s, k) -> (sou base istr, s, k)) list in
      (* On tri la liste avant de la combiner *)
      (* sinon on n'élimine pas les doublons. *)
      let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list in
      (* On combine la liste parce qu'en gwc2, les données peuvent être à  *)
      (* des adresses différentes. NB: on pourrait rassembler les lieux et *)
      (* les sources dans un seul index pour de meilleures performances.   *)
      let list = UpdateData.combine list in
      let string_set1 = ref StringSetAutoComplete.empty in
      (*
      let rec loop place =
        let (_, place) =
          try Api_util.split place ','
          with Not_found -> ("", "")
        in
        if place = "" then ()
        else
          begin
            string_set1 := StringSetAutoComplete.add place !string_set1;
            loop place
          end
      in
      *)
      let () =
        List.iter
          (fun (place, _) ->
             begin
               string_set1 := StringSetAutoComplete.add place !string_set1;
               (*loop place*)
             end)
          list
      in
      let list = StringSetAutoComplete.elements !string_set1 in
      (* Fonction qui à une liste de données retourne la *)
      (* liste de toutes les données commençant par ini. *)
      let nb_res = ref 0 in
      let string_set = ref StrSetAutoComplete.empty in
      let ini = Mutil.tr '_' ' ' n in
      let place_format =
        match p_getenv conf.base_env "places_format" with
        | Some s ->
            (try
               List.fold_right
                 (fun s accu ->
                    match s with
                    | "Subdivision" -> `subdivision :: accu
                    | "Town" -> `town :: accu
                    | "Area code" -> `area_code :: accu
                    | "County" -> `county :: accu
                    | "Region" -> `region :: accu
                    | "Country" -> `country :: accu
                    | _ -> failwith "decode_places_format")
                 (Api_util.explode s ',') []
            with Failure "decode_places_format" -> [])
        | _ -> []
      in
      let len_place_format = List.length place_format in
      let rec reduce l accu mode_dico max_res =
        match l with
        | [] -> ()
        | data :: l ->
            begin
              let k =  Mutil.tr '_' ' ' data in
              let (has_subdivision, i) =
                try
                  (true, String.rindex k ']')
                with Not_found -> (false, 0)
              in
              let k =
                if has_subdivision && place_mode <> Some `subdivision then
                  let s =
                    String.sub k (i + 1) (String.length k - i - 1)
                  in
                  Mutil.strip_all_trailing_spaces s
                else k
              in
              if string_start_with (Name.lower ini) (Name.lower k) then
                begin
                  let tmps =
                    if has_subdivision && place_mode = Some `subdivision then
                      let s =
                        String.sub k (i + 1) (String.length k - i - 1)
                      in
                      List.length (Api_util.explode s ',') + 1
                    else
                      List.length (Api_util.explode k ',') + 1
                  in
                  if len_place_format = 0 || tmps = len_place_format || mode_dico then
                  begin
                    let data =
                      if mode_dico && len_place_format > 0 then
                        let expl_data = Api_util.explode data ',' in
                        let data =
                        List.fold_right
                          (fun format data ->
                            match format with
                            | `town ->
                                (try (List.nth expl_data 4) :: data
                                 with Failure _ -> data)
                            | `area_code ->
                                (try (List.nth expl_data 3) :: data
                                 with Failure _ -> data)
                            | `county ->
                                (try (List.nth expl_data 2) :: data
                                 with Failure _ -> data)
                            | `region ->
                                (try (List.nth expl_data 1) :: data
                                 with Failure _ -> data)
                            | `country ->
                                (try (List.nth expl_data 0) :: data
                                 with Failure _ -> data)
                            | _ -> data)
                          (List.rev place_format) []
                        in
                        String.concat ", " data
                      else
                        data
                    in
                  string_set := StrSetAutoComplete.add data !string_set;
                  incr nb_res;
                  end
                end;
              if !nb_res > max_res then ()
              else reduce l accu mode_dico max_res
            end
      in
      let () = reduce list [] false max_res in
      let base_place = StrSetAutoComplete.elements !string_set in
      let found_res = StrSetAutoComplete.cardinal !string_set in
      let dico_place =
        if found_res >= max_res then []
        else
          begin
            match place_mode with
            | Some pl_mode ->
                let dico = load_dico_lieu conf pl_mode in
                string_set := StrSetAutoComplete.empty;
                reduce dico [] true (max_res - found_res);
                StrSetAutoComplete.elements !string_set;
                (*
                (match pl_mode with
                 | `town -> reduce towns []
                 | `area_code -> reduce area_codes []
                 | `county -> reduce countys []
                 | `region -> reduce regions []
                 | `country -> reduce countrys []
                 | _ -> ())
                *)
            | None -> []
          end;
      in
      let base_place = List.sort Gutil.alphabetic_order base_place in
      let dico_place = List.sort Gutil.alphabetic_order dico_place in
      base_place @ dico_place
      (*
      List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)
      *)
    end
  else
    begin
      if mode = `source then
        begin
          let conf = {(conf) with env = ("data", "src") :: conf.env } in
          let list = UpdateData.get_all_data conf base in
          (* On fait un rev_map (tail-rec) parce que si le nombre de *)
          (* données est trop important, on casser la pile d'appels. *)
          let list = List.rev_map (fun (istr, s, k) -> (sou base istr, s, k)) list in
          (* On tri la liste avant de la combiner *)
          (* sinon on n'élimine pas les doublons. *)
          let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list in
          (* On combine la liste parce qu'en gwc2, les données peuvent être à  *)
          (* des adresses différentes. NB: on pourrait rassembler les lieux et *)
          (* les sources dans un seul index pour de meilleures performances.   *)
          let list = UpdateData.combine list in
          let string_set1 = ref StringSetAutoComplete.empty in
          let () =
            List.iter
              (fun (src, _) ->
                 begin
                   string_set1 := StringSetAutoComplete.add src !string_set1;
                 end)
              list
          in
          let list = StringSetAutoComplete.elements !string_set1 in
          (* Fonction qui à une liste de données retourne la *)
          (* liste de toutes les données commençant par ini. *)
          let nb_res = ref 0 in
          let string_set = ref StrSetAutoComplete.empty in
          let ini = Mutil.tr '_' ' ' n in
          let rec reduce l accu =
            match l with
            | [] -> ()
            | data :: l ->
                begin
                  let k =  Mutil.tr '_' ' ' data in
                  if string_start_with (Name.lower ini) (Name.lower k) then
                    begin
                      string_set := StrSetAutoComplete.add data !string_set;
                      incr nb_res;
                    end;
                  if !nb_res > max_res then ()
                  else reduce l accu
                end
          in
          let () = reduce list [] in
          List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)
        end
      else
        begin
          let _ = load_strings_array base in
          if Name.lower n = "" then []
          else select_start_with_auto_complete conf base mode (get_field mode) max_res n
        end
    end
;;


module IperSetLinkPerson =
  Set.Make
    (struct
      type t = iper ;;
      let compare ip1 ip2 = compare (Adef.int_of_iper ip1) (Adef.int_of_iper ip2);;
     end)
;;

(*
let select_both_link_person conf base ini_n ini_p =
  let find_sn p x = kmp x (sou base (get_surname p)) in
  let find_fn p x = kmp x (sou base (get_first_name p)) in
  let find_str s x = kmp x s in
  let ini_n = name_key_compatible base ini_n in
  let ini_n = code_varenv ini_n in
  let ini_n =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_n []
  in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = code_varenv ini_p in
  let ini_p =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_p []
  in
  let add_maiden p ini_p l =
    let rec loop ifam_l l =
      match ifam_l with
      | [] -> ()
      | ifam :: q ->
          let fam = foi base ifam in
          let ip = Gutil.spouse (get_key_index p) fam in
          let sp = poi base ip in
          if List.for_all (fun s -> find_fn sp s) ini_p then l := ip :: !l;
          loop q l
    in
    if get_sex p = Male then
      loop (Array.to_list (get_family p)) l
  in
  let add_maiden2 p ini_n ini_p l =
    (* On sépare les noms avec tirets ... *)
    let ini_n =
      List.fold_left
        (fun accu s -> (explode s '-') @ accu)
        [] ini_n
    in
    let rec loop ifam_l l =
      match ifam_l with
      | [] -> ()
      | ifam :: q ->
          let fam = foi base ifam in
          let ip = Gutil.spouse (get_key_index p) fam in
          let sp = poi base ip in
          let names =
            sou base (get_surname p) ^ " " ^ sou base (get_surname sp)
          in
          if List.for_all (fun s -> find_str names s) ini_n then
            add_maiden p ini_p l;
          loop q l
    in
    if get_sex p = Male then
      if List.exists (fun s -> find_sn p s) ini_n then
        loop (Array.to_list (get_family p)) l
  in
  let list = ref [] in
  for i = 0 to nb_of_persons base - 1 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    if List.for_all (fun s -> find_sn p s) ini_n then
      begin
        if List.for_all (fun s -> find_fn p s) ini_p then
          list := ip :: !list
        else if maiden_name then add_maiden p ini_p list
      end
    else
      (* On cherche une partie du nom de jeune fille dans les noms donnés. *)
      if maiden_name then add_maiden2 p ini_n ini_p list
  done;
  !list
;;
*)


let select_both_link_person conf base ini_n ini_p max_res =
  let find_sn p x = kmp x (sou base (get_surname p)) in
  let find_fn p x = kmp x (sou base (get_first_name p)) in
  let ini_n = name_key_compatible base ini_n in
  let ini_n = code_varenv ini_n in
  let ini_n =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_n []
  in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = code_varenv ini_p in
  let ini_p =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini_p []
  in
  let rec loop i list nb_res =
    if nb_res < max_res && i < nb_of_persons base then
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      if List.for_all (fun s -> find_sn p s) ini_n then
        if List.for_all (fun s -> find_fn p s) ini_p then
          loop (i + 1) (Adef.iper_of_int i :: list) (nb_res + 1)
        else loop (i + 1) list nb_res
      else loop (i + 1) list nb_res
    else list
  in
  loop 0 [] 0
;;

let select_link_person conf base get_field max_res ini =
  let find p x = kmp x (sou base (get_field p)) in
  let ini = code_varenv ini in
  let ini =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns (decode_varenv (String.sub s 0 index) :: acc)
      else (decode_varenv s :: acc)
    in
    loop ini []
  in
  let rec loop i list nb_res =
    if nb_res < max_res && i < nb_of_persons base then
      if List.for_all (fun s -> find (poi base (Adef.iper_of_int i)) s) ini
      then loop (i + 1) (Adef.iper_of_int i :: list) (nb_res + 1)
      else loop (i + 1) list nb_res
    else list
  in
  loop 0 [] 0
;;

let search_person_list conf base max_res surname first_name =
  let _ = load_strings_array base in
  let (surname, first_name) =
    match (surname, first_name) with
    | (Some n, Some fn) ->
        ((if Name.lower n = "" then None else Some n),
         (if Name.lower fn = "" then None else Some fn))
    | (Some n, None) -> ((if n = "" then None else Some n), None)
    | (None, Some fn) -> (None, (if fn = "" then None else Some fn))
    | (None, None) -> (None, None)
  in
  match (surname, first_name) with
  | (Some n, Some fn) ->
      select_both_start_with_person conf base n fn max_res
  | (Some n, None) ->
      select_start_with_person conf base get_surname max_res n
  | (None, Some fn) ->
      select_start_with_person conf base get_first_name max_res fn
  | (None, None) -> []
;;















(* ******************** SAVE ********************** *)
(*
let select_start_with_auto_complete conf base mode get_field max_res ini =
  let need_whole_list = true in
(*
  let ini =
    if is_surnames then name_key_compatible base ini
    else ini
  in
*)
  let name =
    match mode with
    | `lastname -> persons_of_surname base
    | `firstname -> persons_of_first_name base
  in
  let string_set = ref StrSetAutoComplete.empty in
  (* Si la base est grosse, on fait un vrai start_with. *)
  let () =
    if nb_of_persons base > 100000 then
    else
      begin
          let () =
        (*    let start_k = Mutil.tr '_' ' ' ini in*)
            (* On commence à ? comme ça on fait MAJ et MIN. *)
            let start_k = Mutil.tr '_' ' ' "?" in
            let letter = String.uppercase (String.sub start_k 0 1) in
            match
              try Some (spi_first name letter) with
               Not_found -> None
            with
             | Some istr ->
                let rec loop istr list =
                  if StrSetAutoComplete.cardinal !string_set < max_res then
                    begin
                      let s = sou base istr in
                      let k = name_key_compatible base s in
        (*              if (String.sub k 0 1) = letter then*)
                        if string_incl_start_with (Name.lower ini) (Name.lower k) then
                          begin
                            string_set := StrSetAutoComplete.add (sou base istr) !string_set;
        (*
                          let () =
                            if s <> "?" then
                              let my_list = spi_find name istr in
                              let () =
                                List.iter
                                  (fun ip ->
                                    if is_patched_person base ip then
                                      let p = poi base ip in
                                      let isn = get_field p in
                                      if eq_istr isn istr then
                                        string_set := StrSetAutoComplete.add (sou base isn) !string_set
                                      else ()
                                    else string_set := StrSetAutoComplete.add (sou base istr) !string_set)
                                  my_list
                              in
                              (*
                              let my_list =
                                if conf.use_restrict then
                                  List.fold_left
                                    (fun l ip ->
                                      if is_restricted conf base ip then l
                                      else (ip :: l) )
                                    [] my_list
                                else my_list
                              in
                              List.rev_append my_list list
                              *)
                              ()
                            else ()
                          in
        *)
                          match
                            try Some (spi_next name istr need_whole_list) with
                             Not_found -> None
                          with
                           | Some (istr, dlen) -> loop istr list
                           | None -> ()
                          end
                        else
                          match
                            try Some (spi_next name istr need_whole_list) with
                             Not_found -> None
                          with
                           | Some (istr, dlen) -> loop istr list
                           | None -> ()
        (*              else ()*)
                    end
                in loop istr []
             | None -> ()
          in
        (*
          let () =
            let start_k = Mutil.tr '_' ' ' ini in
            let letter = String.lowercase (String.sub start_k 0 1) in
            match
              try Some (spi_first name letter) with
               Not_found -> None
            with
             | Some istr ->
                let rec loop istr list =
                  if StrSetAutoComplete.cardinal !string_set < max_res then
                    begin
                      let s = sou base istr in
                      let k = name_key_compatible base s in
        (*              if (String.sub k 0 1) = letter then*)
                        if string_incl_start_with (Name.lower ini) (Name.lower k) then
                          let () =
                            if s <> "?" then
                              let my_list = spi_find name istr in
                              let () =
                                List.iter
                                  (fun ip ->
                                    if is_patched_person base ip then
                                      let p = poi base ip in
                                      let isn = get_field p in
                                      if eq_istr isn istr then
                                        string_set := StrSetAutoComplete.add (sou base isn) !string_set
                                      else ()
                                    else string_set := StrSetAutoComplete.add (sou base istr) !string_set)
                                  my_list
                              in
                              (*
                              let my_list =
                                if conf.use_restrict then
                                  List.fold_left
                                    (fun l ip ->
                                      if is_restricted conf base ip then l
                                      else (ip :: l) )
                                    [] my_list
                                else my_list
                              in
                              List.rev_append my_list list
                              *)
                              ()
                            else ()
                          in
                          match
                            try Some (spi_next name istr need_whole_list) with
                              Not_found -> None
                          with
                           | Some (istr, dlen) -> loop istr list
                           | None -> ()
                        else
                          match
                            try Some (spi_next name istr need_whole_list) with
                              Not_found -> None
                          with
                           | Some (istr, dlen) -> loop istr list
                           | None -> ()
        (*              else ()*)
                    end
                in loop istr []
             | None -> ()
          in
        *)
      end
  in
  List.sort Gutil.alphabetic_order (StrSetAutoComplete.elements !string_set)
;;
*)
