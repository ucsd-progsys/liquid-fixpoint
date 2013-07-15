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

(********************************************************************************)
(*********** This module implements the binary interface with SMTLIB2   *********)
(*********** http://www.smt-lib.org/                                    *********)
(*********** http://www.grammatech.com/resource/smt/SMTLIBTutorial.pdf  *********)
(********************************************************************************)


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

module SMTLib2 : ProverArch.SMTSOLVER = struct

let mydebug = false 

let nb_unsat     = ref 0
let nb_pop       = ref 0
let nb_push      = ref 0

(***************************************************************)
(***************** Interaction *********************************)
(***************************************************************)

type context  = unit (* ??? *)

let mkContext _ = () 

(***************************************************************)
(********************** Types **********************************)
(***************************************************************)

type symbol   = Sy.t 
type sort     = So.t
type ast      = E of A.expr | P of A.pred 
type fun_decl = So.t 

let var _ x t = 
  let e = A.eVar x in 
  if So.is_bool t then
    P (A.pBexp e) 
  else 
    E e 

let boundVar me i t 
  = failwith "TODO:SMT.boundVar" (* Z3.mk_bound *)

let stringSymbol _ s 
  = Sy.of_string s

let funcDecl me s ta t 
  = So.t_func 0 (Array.to_list ta ++ t) 

let astString = function 
  | E e -> E.to_string e
  | P p -> P.to_string p

let isBool c a = failwith "TODO:SMT.isBool"

(***********************************************************************)
(*********************** AST Constructors ******************************)
(***********************************************************************)

(* THEORY = QF_UFLIA *)

let mkIntSort _    = So.t_int  
let mkBoolSort _   = So.t_bool
let mkSetSort _    = So.t_int

let mkInt _ i _    = A.eInt
let mkTrue _       = A.pTrue
let mkFalse _      = A.pFalse 

let mkAll me _ _ _ = failwith "TBD:SMT.mkAll"

let mkRel r e1 e2  = A.pAtom (e1, r, e2)

let mkEq me        = mkRel A.Eq
let mkNe           = Z3.mk_distinct 
let mkGt           = Z3.mk_gt
let mkGe           = Z3.mk_ge
let mkLt           = Z3.mk_lt
let mkLe           = Z3.mk_le

let mkApp       = Z3.mk_app
let mkMul       = Z3.mk_mul
let mkAdd       = Z3.mk_add 
let mkSub       = Z3.mk_sub
let mkMod       = Z3.mk_mod 
let mkIte       = Z3.mk_ite

let mkNot       = Z3.mk_not
let mkAnd       = Z3.mk_and 
let mkOr        = Z3.mk_or
let mkImp       = Z3.mk_implies
let mkIff       = Z3.mk_iff

let mkEmptySet  = Z3.mk_empty_set 
let mkSetAdd    = Z3.mk_set_add
let mkSetMem    = Z3.mk_set_member 
let mkSetCup    = Z3.mk_set_union
let mkSetCap    = Z3.mk_set_intersect
let mkSetDif    = Z3.mk_set_difference
let mkSetSub    = Z3.mk_set_subset 

(********************************************************************************)
(************************************ Queries ***********************************)
(********************************************************************************)

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

(* API *)
let print_stats ppf () =
  F.fprintf ppf
    "SMT stats: pushes=%d, pops=%d, unsats=%d \n" 
    !nb_push !nb_pop !nb_unsat 

end
