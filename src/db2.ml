(* $Id: db2.ml,v 5.7 2012-01-20 19:02:51 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

value first_item_pos len =
  20 +
  if len < 8 then 1
  else if len < 1 lsl 22 then 5
  else 9
;

type key2 =
  [ Key of Adef.istr and Adef.istr and int
  | Key0 of Adef.istr and Adef.istr (* to save memory space *) ]
;

value key2_of_key (fn, sn, oc) = if oc = 0 then Key0 fn sn else Key fn sn oc;
