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

let spr = Printf.sprintf

let mydebug = false 

let nb_unsat     = ref 0
let nb_pop       = ref 0
let nb_push      = ref 0

(***************************************************************)
(********************** Types **********************************)
(***************************************************************)

type symbol   = string  (* Sy.t *)
type sort     = string  (* So.t *)
type ast      = string  (* E of A.expr | P of A.pred *) 
type fun_decl = symbol 

type cmd      = Push
              | Pop
              | CheckSat
              | Declare     of symbol * sort list * sort
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

(*******************************************************************)
(*********************** Set Theory ********************************)
(*******************************************************************)

let elt = "Int"
let set = "Set"
let emp = "smt_set_emp"
let add = "smt_set_add"
let cup = "smt_set_cup"
let cap = "smt_set_cap"
let mem = "smt_set_mem"
let dif = "smt_set_dif"
let sub = "smt_set_sub"

let preamble 
  = [ spr "(declare-sort %s)"             set
    ; spr "(declare-fun %s () %s)"        emp set
    ; spr "(declare-fun %s (%s %s) %s)"   add set elt set
    ; spr "(declare-fun %s (%s %s) %s)"   cup set set set
    ; spr "(declare-fun %s (%s %s) %s)"   cap set set set
    ; spr "(declare-fun %s (%s %s) %s)"   dif set set set
    ; spr "(declare-fun %s (%s %s) Bool)" sub set set 
    ; spr "(declare-fun %s (%s %s) Bool)" mem elt set 
    (* TODO: add axioms *)
    ]

let mkSetSort _ _  = set
let mkEmptySet _ _ = emp
let mkSetAdd _ s x = spr "(%s %s %s)" add s x
let mkSetMem _ x s = spr "(%s %s %s)" mem x s 
let mkSetCup _ s t = spr "(%s %s %s)" cup s t
let mkSetCap _ s t = spr "(%s %s %s)" cap s t
let mkSetDif _ s t = spr "(%s %s %s)" dif s t
let mkSetSub _ s t = spr "(%s %s %s)" sub s t

(******************************************************************)
(**************** SMT IO ******************************************)
(** https://raw.github.com/ravichugh/djs/master/src/zzz.ml ********)
(******************************************************************)

(* "z3 -smt2 -in"                   *)
(* "z3 -smtc SOFT_TIMEOUT=1000 -in" *)
(* "z3 -smtc -in MBQI=false"        *)

let cmds     = Misc.hashtbl_of_list [(Z3, "z3 -smt2 -in MODEL=false MODEL.PARTIAL=true")]
let solver   = Z3 
let smt_cmd  = fun () -> H.find cmds solver
let smt_file = fun () -> !Co.out_file ^ ".smt2"

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


(* val interact : context -> cmd -> resp *)
let interact me = function 
  | Declare (x, ts, t) -> 
      let _ = smt_write me <| spr "(declare-fun %s (%s) %s)" x (String.concat " " ts) t in
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
      let _ = smt_write me <| spr "(assert (distinct %s))" (String.concat " " az) in
      Ok


(* API *)
let smt_decl me x ts t 
  = match interact me (Declare (x, ts, t)) with
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

let mkContext _ =
  let cin, cout = Unix.open_process <| smt_cmd  ()        in
  let clog      = open_out          <| smt_file ()        in
  let ctx       = { cin = cin; cout = cout; clog = clog } in
  let _         = List.iter (smt_write ctx) preamble      in
  ctx

(***********************************************************************)
(*********************** AST Constructors ******************************)
(***********************************************************************)


(* THEORY = QF_UFLIA *)

let stringSymbol _ s = s
let astString _ a    = a 
let isBool c a       = failwith "TODO:SMTLib2.isBool"
let boundVar me i t  = failwith "TODO:SMTLib2.boundVar" (* Z3.mk_bound *)

let var me x t =
  let _ = smt_decl me x [] t in  
  x 

let funcDecl me s ta t =
  let _ = smt_decl me s (Array.to_list ta) t in
  s

let mkIntSort _    = "Int"          
let mkBoolSort _   = "Bool"         

let mkInt _ i _    = string_of_int i
let mkTrue _       = "true"
let mkFalse _      = "false" 

let mkAll _ _ _ _  = failwith "TODO:SMTLib2.mkAll"

let mkRel _ r a1 a2 
  = match r with 
  | A.Eq -> spr "(= %s %s)"       a1 a2 
  | A.Ne -> spr "(not (= %s %s))" a1 a2 
  | A.Gt -> spr "(>  %s %s)"      a1 a2 
  | A.Ge -> spr "(>= %s %s)"      a1 a2 
  | A.Lt -> spr "(<  %s %s)"      a1 a2 
  | A.Le -> spr "(<= %s %s)"      a1 a2 



let mkApp _ f = function
  | [] -> f 
  | az -> spr "(%s %s)" f (String.concat " " az)

let opStr = function
  | A.Plus  -> "+"
  | A.Minus -> "-"
  | A.Times -> "*"
  | A.Div   -> "/"
  | A.Mod   -> "mod"

let mkOp op a1 a2
  = spr "(%s %s %s)" (opStr op) a1 a2
  
let mkMul _ = mkOp A.Times  
let mkAdd _ = mkOp A.Plus
let mkSub _ = mkOp A.Minus
let mkMod _ = mkOp A.Mod

let mkIte _ a1 a2 a3 
  = spr "(ite %s %s %s)" a1 a2 a3

let mkNot _ a 
  = spr "(not %s)" a 

let mkAnd _ az  
  = spr "(and %s)" (String.concat " " az) 

let mkOr _ az 
  = spr "(or %s)" (String.concat " " az) 

let mkImp _ a1 a2 
  = spr "(=> %s %s)" a1 a2 

let mkIff _ a1 a2 
  = spr "(= %s %s)" a1 a2 

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
