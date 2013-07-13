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

let nb_unsat		= ref 0
let nb_pop  		= ref 0
let nb_push 		 = ref 0

type smt_context     = Z3.context
type smt_sort        = Z3.sort
type smt_ast         = Z3.ast
type smt_symbol      = Z3.symbol 
type smt_fun_decl    = Z3.func_decl 

(** val smtVar : smt_context -> smt_symbol -> So.t -> smt_ast *)
let smtVar c x t     = Z3.mk_const c x t

(** val smtBoundVar : smt_context -> Sy.t -> So.t -> smt_ast *)
let smtBoundVar      = Z3.mk_bound

(** val smtStringSymbol : smt_context -> string -> smt_symbol *)
let smtStringSymbol = Z3.mk_string_symbol 

(** val smtFuncDecl : smt_context -> smt_symbol -> smt_sort array -> smt_sort -> smt_fun_decl *)
let smtFuncDecl = Z3.mk_func_decl

(** val smtIsBool : smt_context -> smt_ast -> bool *)
let smtIsBool c a =
  a |> Z3.get_sort c   
    |> Z3.sort_to_string c
    |> (=) "bool"

(** NOT USED val smtIsInt : smt_context -> smt_ast -> bool *)
let smtIsInt me a =
  a |> Z3.get_sort me   
    |> Z3.sort_to_string me
    |> (=) "int"

let smt_mkAll = Z3.mk_forall

(** val smt_mkEq : smt_context -> smt_ast -> smt_ast -> smt_ast *)
let smt_mkEq = Z3.mk_eq

(** val smt_mkNe : smt_context -> smt_ast array -> smt_ast      *) 
let smt_mkNe = Z3.mk_distinct 

(** val smt_mkGt : smt_context -> smt_ast -> smt_ast -> smt_ast *) 
let smt_mkGt = Z3.mk_gt

(** val smt_mkGe : smt_context -> smt_ast -> smt_ast -> smt_ast *) 
let smt_mkGe = Z3.mk_ge

(** val smt_mkLt : smt_context -> smt_ast -> smt_ast -> smt_ast *) 
let smt_mkLt = Z3.mk_lt

(** val smt_mkLe : smt_context -> smt_ast -> smt_ast -> smt_ast *) 
let smt_mkLe = Z3.mk_le

(** val smt_mkApp : smt_context -> smt_fun_decl -> smt_ast array -> smt_ast *)
let smt_mkApp = Z3.mk_app

let smt_mkMul = Z3.mk_mul
let smt_mkAdd = Z3.mk_add 
let smt_mkSub = Z3.mk_sub
let smt_mkMod = Z3.mk_mod 
let smt_mkIte = Z3.mk_ite

let smt_mkInt   = Z3.mk_int 
let smt_mkTrue  = Z3.mk_true
let smt_mkFalse = Z3.mk_false
let smt_mkNot   = Z3.mk_not
let smt_mkAnd   = Z3.mk_and 
let smt_mkOr    = Z3.mk_or
let smt_mkImp   = Z3.mk_implies
let smt_mkIff   = Z3.mk_iff

let smt_astString  = Z3.ast_to_string 
let smt_mkIntSort  = Z3.mk_int_sort  
let smt_mkBoolSort = Z3.mk_bool_sort 
let smt_mkContext  = Z3.mk_context_x 

let z3push me =
  let _ = nb_push += 1 in
  let _ = BS.time "Z3.push" Z3.push me in
  () 

let z3pop me =
  let _ = incr nb_pop in
  BS.time "Z3.pop" (Z3.pop me) 1 


(* Z3 API *)
let z3unsat =  
  let us_ref = ref 0 in
  fun me ->
    let _  = if mydebug then (Printf.printf "[%d] UNSAT 1 " (us_ref += 1); flush stdout) in
    let rv = (BS.time "Z3.check" Z3.check me) = Z3.L_FALSE in
    let _  = if mydebug then (Printf.printf "UNSAT 2 \n"; flush stdout) in
    let _  = if rv then ignore (nb_unsat += 1) in 
    rv

(* Z3 API *)
let smt_AssertAxiom me p =
  Co.bprintf mydebug "@[Pushing axiom %s@]@." (smt_astString me p); 
  BS.time "Z3 assert axiom" (Z3.assert_cnstr me) p;
  asserts (not (z3unsat me)) "ERROR: Axiom makes background theory inconsistent!"

(* Z3 API *)
let smt_bracket me f = Misc.bracket (fun _ -> z3push me) (fun _ -> z3pop me) f

(* Z3 API *)
let smt_assert me ps = List.iter (fun p -> BS.time "Z3.ass_cst" (Z3.assert_cnstr me) p) ps

(* Z3 API *)
let smt_valid me p = 
  smt_bracket me begin fun _ ->
    smt_assert me [Z3.mk_not me p];
    BS.time "unsat" z3unsat me 
  end

(* Z3 API *)
let smt_contra me p = 
  smt_bracket me begin fun _ ->
    smt_assert me [p];
    BS.time "unsat" z3unsat me 
  end
 
