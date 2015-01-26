(*
 * Copyright © 2009 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *)

module So = Ast.Sort
module Sy = Ast.Symbol

open ProverArch
open FixMisc.Ops

(***************************************************************************)
(********************* NAMES (independent of SMT) **************************)
(***************************************************************************)

(********************* Sets ************************************************)

let set_tycon  = So.tycon "Set_Set"
let t_set a    = So.t_app set_tycon [a]

(* API *)
let emp0 = ( Sy.of_string "Set_empty"
           , So.t_func 1 [So.t_int; t_set (So.t_generic 0)] )

let emp = ( Sy.of_string "Set_emp"
          , So.t_func 1 [t_set (So.t_generic 0); So.t_bool] )

let sng = ( Sy.of_string "Set_sng"
          , So.t_func 1 [So.t_generic 0; t_set (So.t_generic 0)] )

let mem = ( Sy.of_string "Set_mem"
          , So.t_func 1 [So.t_generic 0; t_set (So.t_generic 0); So.t_bool] )

let cup = ( Sy.of_string "Set_cup"
          , So.t_func 1 [t_set (So.t_generic 0); t_set (So.t_generic 0); t_set (So.t_generic 0)])

let cap = ( Sy.of_string "Set_cap"
          , So.t_func 1 [t_set (So.t_generic 0); t_set (So.t_generic 0); t_set (So.t_generic 0)])

let dif = ( Sy.of_string "Set_dif"
          , So.t_func 1 [t_set (So.t_generic 0); t_set (So.t_generic 0); t_set (So.t_generic 0)])

let sub = ( Sy.of_string "Set_sub" 
          , So.t_func 1 [t_set (So.t_generic 0); t_set (So.t_generic 0); So.t_bool] )

(********************* Maps ************************************************)

let map_tycon  = So.tycon "Map_t"

let t_map k v  = So.t_app map_tycon [k; v]

let select     = let k = So.t_generic 0 in
                 let v = So.t_generic 1 in
                 ( Sy.of_string "Map_select"
                 , So.t_func 2 [t_map k v; k; v] )

let store      = let k = So.t_generic 0 in
                 let v = So.t_generic 1 in
                 ( Sy.of_string "Map_store"
                 , So.t_func 2 [t_map k v; k; v; t_map k v] )


(** WARNING: DO NOT PUT INSIDE MakeTheory; adds SMT dependency ***)

(* API *)
let is_interp t = t = set_tycon || t = map_tycon

(* API *)
let interp_syms _ 
  = []
    |> (!Constants.set_theory <?> (++) [emp0; emp; sng; mem; cup; cap; dif; sub])
    |> (!Constants.map_theory <?> (++) [select; store])


(***************************************************************************)
(***************************************************************************)
(***************************************************************************)

module MakeTheory(SMT : SMTSOLVER): 
  (THEORY with type context = SMT.context 
          and  type sort    = SMT.sort
          and  type ast     = SMT.ast) 
  = struct 

type context = SMT.context
type sort    = SMT.sort
type ast     = SMT.ast


type appDef  = { sy_name  : Sy.t
               ; sy_sort  : So.t
               ; sy_emb   : SMT.context -> SMT.sort list -> SMT.ast list -> SMT.ast
               }

type sortDef = { so_name  : Ast.Sort.tycon
               ; so_arity : int
               ; so_emb   : SMT.context -> SMT.sort list -> SMT.sort 
               }

(* API *)
let sort_name d = d.so_name
let sym_name d  = d.sy_name
let sym_sort d  = d.sy_sort

(***************************************************************************)
(********* Wrappers Around Z3 Constructors For Last-Minute Checking ********)
(***************************************************************************)

let app_sort_arity def = match So.func_of_t def.sy_sort with
  | Some (n,_,_) -> n
  | None         -> assertf "Theories: app with non-function symbol %s" 
                    (Sy.to_string def.sy_name)

let check_app_arities def tArgs eArgs = match So.func_of_t def.sy_sort with
  | Some (n, ts,_) 
     -> asserts (n = List.length tArgs)  
          "Theories: app with mismatched sorts %s" (Sy.to_string def.sy_name);
        asserts (List.length ts = List.length eArgs) 
          "Theories: app with mismatched args %s" (Sy.to_string def.sy_name) 
  | None         
     -> assertf "Theories: app with non-function symbol %s" 
          (Sy.to_string def.sy_name)


(* API *)
let mk_thy_app def c ts es = 
  check_app_arities def ts es;
  def.sy_emb c ts es

(* API *)
let mk_thy_sort def c ts = 
  asserts (List.length ts = def.so_arity) 
    "Theories: app with mismatched sorts %s" (So.tycon_string def.so_name);
  def.so_emb c ts 


(***************************************************************************)
(******************** Theory of Sets ***************************************)
(***************************************************************************)


(**************** EMBEDDING (dependent of SMT) **************************)

let set_t : sortDef = 
  { so_name  = set_tycon 
  ; so_arity = 1 
  ; so_emb   = fun c -> function 
                 [t] -> SMT.mkSetSort c t
                 | _ -> assertf "Set_set: type mismatch"
  }  

let set_empty : appDef  = 
  { sy_name  = fst emp0
  ; sy_sort  = snd emp0 
  ; sy_emb   = fun c ts es -> match ts, es with
                 | [t], [_] -> SMT.mkEmptySet c t
                 | _        -> assertf "Set_empty: type mismatch"
  }

let set_emp : appDef  = 
  { sy_name  = fst emp 
  ; sy_sort  = snd emp 
  ; sy_emb   = fun c ts es -> match ts, es with
                 | [t], [e] -> SMT.mkRel c Ast.Eq e (SMT.mkEmptySet c t)
                 | _        -> assertf "Set_emp: type mismatch"
  }

let set_sng : appDef  = 
  { sy_name = fst sng 
  ; sy_sort = snd sng  
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e] -> SMT.mkSetAdd c (SMT.mkEmptySet c t) e
                 | _        -> assertf "Set_sng: type mismatch"
  }


let set_mem : appDef  = 
  { sy_name = fst mem 
  ; sy_sort = snd mem
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [_], [e;es] -> SMT.mkSetMem c e es 
                 | _           -> assertf "Set_mem: type mismatch"
  }

let set_cup : appDef  = 
  { sy_name = fst cup 
  ; sy_sort = snd cup
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e1;e2] -> SMT.mkSetCup c e1 e2
                 | _            -> assertf "Set_cup: type mismatch"
  }

let set_cap : appDef  = 
  { sy_name = fst cap 
  ; sy_sort = snd cap 
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e1;e2] -> SMT.mkSetCap  c e1 e2
                 | _            -> assertf "Set_cap: type mismatch"
  }

let set_dif : appDef  = 
  { sy_name = fst dif 
  ; sy_sort = snd dif
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e1;e2] -> SMT.mkSetDif c e1 e2 
                 | _            -> assertf "Set_dif: type mismatch"
  }

let set_sub : appDef =
  { sy_name = fst sub 
  ; sy_sort = snd sub
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e1;e2] -> SMT.mkSetSub c e1 e2 
                 | _            -> assertf "Set_dif: type mismatch"
  }

(* API *)
let theory_set 
  = ([ set_t ]
    ,[ set_emp 
     ; set_empty 
     ; set_sng 
     ; set_mem 
     ; set_cup 
     ; set_cap 
     ; set_dif 
     ; set_sub ])

(***************************************************************************)
(******************** Theory of Maps ***************************************)
(***************************************************************************)

(**************** EMBEDDING (dependent of SMT) **************************)

let map_t : sortDef = 
  { so_name  = map_tycon 
  ; so_arity = 2 
  ; so_emb   = fun c -> function 
                 | [k; v] -> SMT.mkMapSort c k v 
                 | _      -> assertf "Map_t: type mismatch"
  }  

let map_select : appDef  = 
  { sy_name = fst select 
  ; sy_sort = snd select 
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [_; _], [m; k] -> SMT.mkMapSelect c m k 
                 | _           -> assertf "Map_select: type mismatch"
  }

let map_store : appDef  = 
  { sy_name = fst store 
  ; sy_sort = snd store 
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [_; _], [m; k; v] -> SMT.mkMapStore c m k v 
                 | _           -> assertf "Map_store: type mismatch"
  }

(* API *)
let theory_map
  = ([map_t], [map_select; map_store])

(***************************************************************************)
(** Composing Theories *****************************************************)
(***************************************************************************)

(* API *)
let theories () = 
  let add_thy (t1,s1) (t2,s2) = (t1 ++ t2, s1 ++ s2) in
  ([], [])
  |> (!Constants.set_theory <?> add_thy theory_set)
  |> (!Constants.map_theory <?> add_thy theory_map)


(* ideally:
let is_interp t = 
  let sorts = fst theories |>: (fun x -> x.so_name) in
  List.mem t sorts
*)


(* JUNK

let interp_syms_set 
  = [emp0; emp; sng; mem; cup; cap; dif; sub]

let interp_syms_map
  = [sel; sto] 

(* API *)
let interp_syms _ 
  = []
    |> (!Constants.set_theory <?> add_syms interp_syms_set)
    |> (!Constants.map_theory <?> add_syms interp_syms_map)
*)

end
