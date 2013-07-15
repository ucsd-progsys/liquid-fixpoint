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
(* module SMT  = SmtZ3.SMTZ3 *)

open ProverArch
open FixMisc.Ops

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
(******************** Theory of Sets ***************************************)
(***************************************************************************)

let set_tycon  = So.tycon "Set_Set"
let t_set a    = So.t_app set_tycon [a]

let set_set : sortDef = 
  { so_name  = set_tycon 
  ; so_arity = 1 
  ; so_emb   = fun c -> function 
                 [t] -> SMT.mkSetSort c t
                 | _ -> assertf "Set_set: type mismatch"
  }  

let set_emp : appDef  = 
  { sy_name  = Sy.of_string "Set_emp"
  ; sy_sort  = So.t_func 1 [t_set (So.t_generic 0); So.t_bool]
  ; sy_emb   = fun c ts es -> match ts, es with
                 | [t], [e] -> SMT.mkRel c Ast.Eq e (SMT.mkEmptySet c t)
                 | _        -> assertf "Set_emp: type mismatch"
  }

let set_sng : appDef  = 
  { sy_name = Sy.of_string "Set_sng"
  ; sy_sort = So.t_func 1 [So.t_generic 0; t_set (So.t_generic 0)] 
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e] -> SMT.mkSetAdd c (SMT.mkEmptySet c t) e
                 | _        -> assertf "Set_sng: type mismatch"
  }


let set_mem : appDef  = 
  { sy_name = Sy.of_string "Set_mem"
  ; sy_sort = So.t_func 1 [So.t_generic 0; t_set (So.t_generic 0); So.t_bool] 
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e;es] -> SMT.mkSetMem c e es 
                 | _           -> assertf "Set_mem: type mismatch"
  }

let set_cup : appDef  = 
  { sy_name = Sy.of_string "Set_cup"
  ; sy_sort = So.t_func 1 [t_set (So.t_generic 0); t_set (So.t_generic 0); t_set (So.t_generic 0)]
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e1;e2] -> SMT.mkSetCup c [| e1; e2 |] 
                 | _            -> assertf "Set_cup: type mismatch"
  }

let set_cap : appDef  = 
  { sy_name = Sy.of_string "Set_cap"
  ; sy_sort = So.t_func 1 [t_set (So.t_generic 0); t_set (So.t_generic 0); t_set (So.t_generic 0)] 
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e1;e2] -> SMT.mkSetCap  c [| e1; e2 |] 
                 | _            -> assertf "Set_cap: type mismatch"
  }

let set_dif : appDef  = 
  { sy_name = Sy.of_string "Set_dif"
  ; sy_sort = So.t_func 1 [t_set (So.t_generic 0); t_set (So.t_generic 0); t_set (So.t_generic 0)]
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e1;e2] -> SMT.mkSetDif c e1 e2 
                 | _            -> assertf "Set_dif: type mismatch"
  }

let set_sub : appDef =
  { sy_name = Sy.of_string "Set_sub"
  ; sy_sort = So.t_func 1 [t_set (So.t_generic 0); t_set (So.t_generic 0); So.t_bool] 
  ; sy_emb  = fun c ts es -> match ts, es with
                 | [t], [e1;e2] -> SMT.mkSetSub c e1 e2 
                 | _            -> assertf "Set_dif: type mismatch"
  }

(* API *)
let set_theory = ([set_set], [set_emp; 
                              set_sng; 
                              set_mem; 
                              set_cup; 
                              set_cap; 
                              set_dif; 
                              set_sub])

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

(* API *)
let theories () = set_theory

(* API *)
let is_interp t = (t = set_tycon)

end
