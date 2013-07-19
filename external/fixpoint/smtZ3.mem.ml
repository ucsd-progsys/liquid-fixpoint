(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
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

(* This file is part of the LiquidC Project *)

module H  = Hashtbl
module F  = Format
module Co = Constants
module BS = BNstats
module A  = Ast
module Sy = A.Symbol
module So = A.Sort
module SM = Sy.SMap
module P  = A.Predicate
module E  = A.Expression
module Misc = FixMisc open Misc.Ops
module SSM = Misc.StringMap
module Th = Theories

module SMTZ3 : ProverArch.SMTSOLVER = struct

let mydebug = false 

(********************************************************************************)
(************ SMT INTERFACE *****************************************************) 
(********************************************************************************)

let nb_unsat     = ref 0
let nb_pop       = ref 0
let nb_push      = ref 0

type context     = Z3.context
type symbol      = Z3.symbol
type sort        = Z3.sort
type ast         = Z3.ast
type fun_decl    = Z3.func_decl 

let var          = Z3.mk_const 
let boundVar     = Z3.mk_bound
let stringSymbol = Z3.mk_string_symbol 
let funcDecl     = Z3.mk_func_decl

let isBool c a =
  a |> Z3.get_sort c   
    |> Z3.sort_to_string c
    |> (=) "bool"

let isInt me a =
  a |> Z3.get_sort me   
    |> Z3.sort_to_string me
    |> (=) "int"

let mkAll me = Z3.mk_forall me 0 [||]

let mkRel c r a1 a2 
  = match r with
  | A.Eq -> Z3.mk_eq c          a1  a2  
  | A.Ne -> Z3.mk_distinct c [| a1; a2 |]
  | A.Gt -> Z3.mk_gt c          a1  a2 
  | A.Ge -> Z3.mk_ge c          a1  a2
  | A.Lt -> Z3.mk_lt c          a1  a2
  | A.Le -> Z3.mk_le c          a1  a2

let mkApp c f az  = Z3.mk_app c f (Array.of_list az)
let mkMul c a1 a2 = Z3.mk_mul c [| a1; a2|]
let mkAdd c a1 a2 = Z3.mk_add c [| a1; a2|]
let mkSub c a1 a2 = Z3.mk_sub c [| a1; a2|]
let mkMod = Z3.mk_mod 
let mkIte = Z3.mk_ite

let mkInt      = Z3.mk_int 
let mkTrue     = Z3.mk_true
let mkFalse    = Z3.mk_false
let mkNot      = Z3.mk_not
let mkAnd c az = Z3.mk_and c (Array.of_list az) 
let mkOr c az  = Z3.mk_or c  (Array.of_list az) 
let mkImp      = Z3.mk_implies
let mkIff      = Z3.mk_iff
let astString  = Z3.ast_to_string 
let mkIntSort  = Z3.mk_int_sort  
let mkBoolSort = Z3.mk_bool_sort 
let mkSetSort  = Z3.mk_set_sort  
let mkEmptySet = Z3.mk_empty_set 
let mkSetAdd   = Z3.mk_set_add
let mkSetMem   = Z3.mk_set_member 
let mkSetCup   = fun me s1 s2 -> Z3.mk_set_union     me [| s1; s2 |]
let mkSetCap   = fun me s1 s2 -> Z3.mk_set_intersect me [| s1; s2 |]
let mkSetDif   = Z3.mk_set_difference
let mkSetSub   = Z3.mk_set_subset 
let mkContext  = Z3.mk_context_x 

(*********************************************************)

let z3push me =
  let _ = nb_push += 1 in
  let _ = BS.time "Z3.push" Z3.push me in
  () 

let z3pop me =
  let _ = incr nb_pop in
  BS.time "Z3.pop" (Z3.pop me) 1 


(* Z3 API *)
let unsat =  
  let us_ref = ref 0 in
  fun me ->
    let _  = if mydebug then (Printf.printf "[%d] UNSAT 1 " (us_ref += 1); flush stdout) in
    let rv = (BS.time "Z3.check" Z3.check me) = Z3.L_FALSE in
    let _  = if mydebug then (Printf.printf "UNSAT 2 \n"; flush stdout) in
    let _  = if rv then ignore (nb_unsat += 1) in 
    rv

(* API *)
let assertAxiom me p =
  Co.bprintf mydebug "@[Pushing axiom %s@]@." (astString me p); 
  BS.time "Z3 assert axiom" (Z3.assert_cnstr me) p;
  asserts (not (unsat me)) "ERROR: Axiom makes background theory inconsistent!"

(* API *)
let assertDistinct me xs =
  xs |> Array.of_list |> Z3.mk_distinct me |> assertAxiom me

(* Z3 API *)
let bracket me f = Misc.bracket (fun _ -> z3push me) (fun _ -> z3pop me) f

(* Z3 API *)
let assertPreds me ps = List.iter (fun p -> BS.time "Z3.ass_cst" (Z3.assert_cnstr me) p) ps

(* Z3 API *)
let valid me p = 
  bracket me begin fun _ ->
    assertPreds me [Z3.mk_not me p];
    BS.time "unsat" unsat me 
  end

(* Z3 API *)
let contra me p = 
  bracket me begin fun _ ->
    assertPreds me [p];
    BS.time "unsat" unsat me 
  end

(* API *)
let print_stats ppf () =
  F.fprintf ppf
    "SMT stats: pushes=%d, pops=%d, unsats=%d \n" 
    !nb_push !nb_pop !nb_unsat 

end
