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
type fun_decl = {fun_name : Sy.t; fun_sort : So.t}

let var _ x t = 
  let e = A.eVar x in 
  if So.is_bool t then
    P (A.pBexp e) 
  else 
    E e 

let boundVar me i t 
  = failwith "TODO:SMTLib2.boundVar" (* Z3.mk_bound *)

let stringSymbol _ s 
  = Sy.of_string s

let funcDecl me s ta t 
  = { fun_name = s
    ; fun_sort = So.t_func 0 (Array.to_list ta ++ [t])
    }

let astString _ = function 
  | E e -> E.to_string e
  | P p -> P.to_string p

let isBool c a = failwith "TODO:SMTLib2.isBool"

(***********************************************************************)
(*********************** AST Constructors ******************************)
(***********************************************************************)

(* THEORY = QF_UFLIA *)

let mkIntSort _    = So.t_int  
let mkBoolSort _   = So.t_bool
let mkSetSort _ _  = failwith "TODO:SMTLib2.mkSetSort"

let mkInt _ i _    = E (A.eInt i)
let mkTrue _       = P A.pTrue
let mkFalse _      = P A.pFalse 

let mkAll _ _ _ _ = failwith "TBD:SMT.mkAll"

let getExpr = function
  | E e -> e
  | _   -> assertf "smtLIB2.getExpr"

let getPred = function
  | P p -> p
  | _   -> assertf "smtLIB2.getPred"

let mkRel _ r a1 a2 
  = P (A.pAtom (getExpr a1, r, getExpr a2))

let mkApp _ f az 
  = E (A.eApp (f.fun_name, List.map getExpr az))

let mkOp op a1 a2
  = E (A.eBin (getExpr a1, op, getExpr a2))

let mkMul _ = mkOp A.Times  
let mkAdd _ = mkOp A.Plus
let mkSub _ = mkOp A.Minus
let mkMod _ = mkOp A.Mod

let mkIte _ a1 a2 a3 
  = E (A.eIte (getPred a1, getExpr a2, getExpr a3))

let mkNot _ a 
  = P (A.pNot (getPred a))

let mkAnd _ az  
  = P (A.pAnd (List.map getPred az))

let mkOr _ az 
  = P (A.pOr (List.map getPred az))

let mkImp _ a1 a2 
  = P (A.pImp (getPred a1, getPred a2))

let mkIff _ a1 a2 
  = P (A.pIff (getPred a1, getPred a2))

let mkEmptySet _ = failwith "TODO:SMTLIB2.set-theory" 
let mkSetAdd   _ = failwith "TODO:SMTLIB2.set-theory"
let mkSetMem   _ = failwith "TODO:SMTLIB2.set-theory"
let mkSetCup   _ = failwith "TODO:SMTLIB2.set-theory"
let mkSetCap   _ = failwith "TODO:SMTLIB2.set-theory"
let mkSetDif   _ = failwith "TODO:SMTLIB2.set-theory" 
let mkSetSub   _ = failwith "TODO:SMTLIB2.set-theory"

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


(* API *)
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
  (* Co.bprintf mydebug "@[Pushing axiom %s@]@." (astString me p); *)
  BS.time "Z3 assert axiom" (Z3.assert_cnstr me) p;
  asserts (not (unsat me)) "ERROR: Axiom makes background theory inconsistent!"

(* API *)
let assertDistinct me xs = failwith "TODO:SMTLIB2.assertDistinct"

(* API *)
let bracket me f = Misc.bracket (fun _ -> z3push me) (fun _ -> z3pop me) f

(* API *)
let assertPreds me ps = List.iter (fun p -> BS.time "Z3.ass_cst" (Z3.assert_cnstr me) p) ps

(* API *)
let print_stats ppf () =
  F.fprintf ppf
    "SMT stats: pushes=%d, pops=%d, unsats=%d \n" 
    !nb_push !nb_pop !nb_unsat 

end
