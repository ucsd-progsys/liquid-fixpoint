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
(********************** Types **********************************)
(***************************************************************)

type symbol   = Sy.t 
type sort     = So.t
type ast      = E of A.expr | P of A.pred 
type fun_decl = {fun_name : Sy.t; fun_sort : So.t}

type cmd      = Push
              | Pop
              | CheckSat
              | Declare     of symbol * sort
              | AssertCnstr of ast
              | Distinct    of ast list

type resp     = Ok 
              | Sat 
              | Unsat 
              | Error of string

type solver   = Z3

type context  = { cin  : in_channel
                ; cout : out_channel
                ; clog : out_channel }


(******************************************************************)
(**************** SMT IO ******************************************)
(** https://raw.github.com/ravichugh/djs/master/src/zzz.ml ********)
(******************************************************************)

(* "z3 -smt2 -in"                   *)
(* "z3 -smtc SOFT_TIMEOUT=1000 -in" *)
(* "z3 -smtc -in MBQI=false"        *)

let cmds     = Misc.hashtbl_of_list [(Z3, "z3 -smt2 -in MBQI=false")]
let solver   = Z3 
let smt_cmd  = fun () -> H.find cmds solver
let smt_file = fun () -> !Co.out_file ^ ".smt2"

let mkContext =
  let cin, cout = Unix.open_process <| smt_cmd  () in
  let clog      = open_out          <| smt_file () in
  { cin = cin; cout = cout; clog = clog }

let smt_write_raw me s = 
  output_now me.clog s; 
  output_now me.cout s

let smt_read_raw me = 
  input_line me.cin

let smt_write me ?nl:(nl=true) ?tab:(tab=false) s =
  let pre = if tab then "    " else "" in
  let suf = if nl  then "\n"   else "" in
  smt_write_raw me (pre^s^suf)

let rec smt_read me
  = match smt_read_raw me with
  | "sat"     -> Sat
  | "unsat"   -> Unsat
  | "success" -> smt_read me 
  | s         -> Error s

let spr = Printf.sprintf

(* val interact : context -> cmd -> resp *)
let interact me = function 
  | Declare (x, t) -> 
      let _ = smt_write me <| spr "(declare-fun %s %s)" x t in
      Ok 
  | Push -> 
      let _ = smt_write me <|     "(push)" in
      Ok
  | Pop -> 
      let _ = smt_write me <|     "(pop)" in
      Ok
  | CheckSat -> 
      let _ = smt_write me <|     "(check-sat)" in
      smt_read me 
  | AssertCnstr a -> 
      let _ = smt_write me <| spr "(assert %s)" a in
      Ok
  | Distinct az -> 
      let _ = smt_write me <| spr "(distinct %s)" (String.concat " " az) in
      Ok

(* {{{ 
  
(***** Scoping ****************************************************************)

let curVars = Stack.create ()

let _ = Stack.push [] curVars

let varInScope x =
  let b = ref false in
  Stack.iter (fun l -> if List.mem x l then b := true) curVars;
  !b

let pushScope () =
  smt_write ~nl:(not !doingExtract) "(push) ";
  incr depth;
  Stack.push [] curVars;
  ()

let popScope () =
  decr depth;
  smt_write "(pop)";
  ignore (Stack.pop curVars);
  ()

let inNewScope f =
  pushScope ();
  try
    let x = f () in
    let _ = popScope () in
    x
  with e ->
    let _ = popScope () in (* in case exception is caught later *)
    raise e

(***** Querying ***************************************************************)

let queryCount = ref 0

let checkSat cap =
  let rec readSat () =
    match smt_read () with
      | "sat"     -> "sat", true
      | "unsat"   -> "unsat", false
      | "unknown" -> "unknown", true
      | "success" -> readSat ()
      | s         -> z3err (spr "Zzz.checkSat: read weird string [%s]" s)
  in
  (* always print \n after (check-sat), to make sure z3 reads from pipe *)
  smt_write ~tab:(not !doingExtract) ~nl:true "(check-sat)";
  incr queryCount;
  incrQueryRootCount ();
  let (s,b) = readSat () in
  smt_write (spr "; [%s] query %d (%s)" s !queryCount cap);
  b

let checkSat cap =
  BNstats.time "Zzz.checkSat" checkSat cap

let checkValid cap p =
  pushScope ();
  if p = pFls then () (* smt_write "(assert true)" *)
  else if !doingExtract then
    smt_write ~tab:false ~nl:false (spr "(assert (not %s))" (embedForm p))
  else
    smt_write (spr "(assert (not\n%s  %s))" (indent()) (embedForm p))
  ;
  let sat = checkSat cap in
  popScope ();
  not sat

(***** Adding variables and formulas ******************************************)

let assertFormula f =
  if f <> pTru then
    smt_write (spr "(assert\n%s  %s)" (indent()) (embedForm (simplify f)))

let addBinding x t =
  smt_write (spr "(declare-fun %s () DVal) ; depth %d" x !depth);
  smt_write (spr "(assert (not (= %s bot)))" x);
  if varInScope x then Log.warn (spr "already in scope in logic: %s\n" x);
  Stack.push (x :: Stack.pop curVars) curVars;
  assertFormula (applyTyp t (wVar x));
  if Str.string_match (Str.regexp "^end_of_") x 0 then begin
    smt_write "";
    smt_write (String.make 80 ';');
    smt_write (String.make 80 ';');
    smt_write (String.make 80 ';');
    smt_write "";
  end;
  ()

}}} *)


(***************************************************************************)
(***************************************************************************)
(***************************************************************************)



(* API *)
let smt_decl me x t 
  = match interact me (Declare (x, t)) with
  | Ok -> ()
  | _  -> assertf "crash: SMTLIB2 smt_decl"

(* API *)
let smt_push me 
  = match interact me Push with
  | Ok -> (nb_push += 1); () 
  | _  -> assertf "crash: SMTLIB2 smt_push"

(* API *)
let smt_pop me 
  = match interact me Pop with
  | Ok -> (nb_pop += 1); () 
  | _  -> assertf "crash: SMTLIB2 smt_pop"

(* API *)
let smt_check_unsat me 
  = match interact me CheckSat with
  | Unsat -> true
  | Sat   -> false
  | _     -> assertf "crash: SMTLIB2 smt_check_unsat"

(* API *)
let smt_assert_cnstr me p 
  = match interact me (AssertCnstr p) with
  | Ok -> ()
  | _  -> assertf "crash: SMTLIB2 smt_assert_cnstr"

(* API *)
let smt_assert_distinct me az
  = match interact me (Distinct az) with
  | Ok -> ()
  | _  -> assertf "crash: SMTLIB2 smt_assert_distinct"


(***************************************************************)
(********************** Constructors ***************************)
(***************************************************************)

let var me x t =
  let _ = smt_decl me x t in  
  let e = A.eVar x        in 
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

(*******************************************************************)
(*********************** Queries ***********************************)
(*******************************************************************)

let us_ref = ref 0

(* API *)
let unsat me =  
  let _  = if mydebug then begin 
              Printf.printf "[%d] UNSAT 1 " (us_ref += 1);
              flush stdout
           end 
  in
  let rv = BS.time "SMT.check_unsat" smt_check_unsat me               in
  let _  = if mydebug then (Printf.printf "UNSAT 2 \n"; flush stdout) in
  let _  = if rv then ignore (nb_unsat += 1) in 
  rv

(* API *)
let assertAxiom me p
  = (* Co.bprintf mydebug "@[Pushing axiom %s@]@." (astString me p); *)
    BS.time "Z3 assert axiom" (smt_assert_cnstr me) p;
    asserts (not (unsat me)) "ERROR: Axiom makes background theory inconsistent!"

(* API *)
let assertDistinct 
  = smt_assert_distinct

(* API *)
let bracket me f 
  = Misc.bracket (fun _ -> smt_push me) (fun _ -> smt_pop me) f

(* API *)
let assertPreds me ps 
  = List.iter (fun p -> BS.time "assertPreds" (smt_assert_cnstr me) p) ps

(* API *)
let print_stats ppf () =
  F.fprintf ppf "SMT stats: pushes=%d, pops=%d, unsats=%d \n" 
    !nb_push !nb_pop !nb_unsat

end
