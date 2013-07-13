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


let mydebug = false 

(********************************************************************************)
(************ SMT INTERFACE *****************************************************) 
(********************************************************************************)

let nb_unsat     = ref 0
let nb_pop       = ref 0
let nb_push      = ref 0

type context     = Z3.context
type sort        = Z3.sort
type ast         = Z3.ast
type symbol      = Z3.symbol 
type fun_decl    = Z3.func_decl 

(** val var : smt_context -> smt_symbol -> So.t -> ast *)
let var          = Z3.mk_const 

(** val smtBoundVar : smt_context -> Sy.t -> So.t -> ast *)
let boundVar      = Z3.mk_bound

(** val stringSymbol : smt_context -> string -> smt_symbol *)
let stringSymbol = Z3.mk_string_symbol 

(** val smtFuncDecl : smt_context -> smt_symbol -> smt_sort array -> smt_sort -> smt_fun_decl *)
let funcDecl = Z3.mk_func_decl

(** val isBool : smt_context -> ast -> bool *)
let isBool c a =
  a |> Z3.get_sort c   
    |> Z3.sort_to_string c
    |> (=) "bool"

(** NOT USED val smtIsInt : smt_context -> ast -> bool *)
let smt_isInt me a =
  a |> Z3.get_sort me   
    |> Z3.sort_to_string me
    |> (=) "int"

let mkAll = Z3.mk_forall

(** val mkEq : smt_context -> ast -> ast -> ast *)
let mkEq = Z3.mk_eq

(** val mkNe : smt_context -> ast array -> ast      *) 
let mkNe = Z3.mk_distinct 

(** val mkGt : smt_context -> ast -> ast -> ast *) 
let mkGt = Z3.mk_gt

(** val mkGe : smt_context -> ast -> ast -> ast *) 
let mkGe = Z3.mk_ge

(** val mkLt : smt_context -> ast -> ast -> ast *) 
let mkLt = Z3.mk_lt

(** val mkLe : smt_context -> ast -> ast -> ast *) 
let mkLe = Z3.mk_le

(** val mkApp : smt_context -> smt_fun_decl -> ast array -> ast *)
let mkApp = Z3.mk_app

let mkMul = Z3.mk_mul
let mkAdd = Z3.mk_add 
let mkSub = Z3.mk_sub
let mkMod = Z3.mk_mod 
let mkIte = Z3.mk_ite

let mkInt   = Z3.mk_int 
let mkTrue  = Z3.mk_true
let mkFalse = Z3.mk_false
let mkNot   = Z3.mk_not
let mkAnd   = Z3.mk_and 
let mkOr    = Z3.mk_or
let mkImp   = Z3.mk_implies
let mkIff   = Z3.mk_iff

let astString  = Z3.ast_to_string 
let mkIntSort  = Z3.mk_int_sort  
let mkBoolSort = Z3.mk_bool_sort 
let mkContext  = Z3.mk_context_x 

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

(* Z3 API *)
let assertAxiom me p =
  Co.bprintf mydebug "@[Pushing axiom %s@]@." (astString me p); 
  BS.time "Z3 assert axiom" (Z3.assert_cnstr me) p;
  asserts (not (unsat me)) "ERROR: Axiom makes background theory inconsistent!"

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
