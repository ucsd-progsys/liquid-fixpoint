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
 *
 *)

(* Theories API *)

module type THEORY = sig
  
  type smt_context
  type smt_sort
  type smt_ast
  type appDef 
  type sortDef 

  val sym_sort    : appDef  -> Ast.Sort.t
  val sym_name    : appDef  -> Ast.Symbol.t
  val sort_name   : sortDef -> Ast.Sort.tycon
  val theories    : unit -> sortDef list * appDef list
  val mk_thy_sort : sortDef -> smt_context -> smt_sort list -> smt_sort
  val mk_thy_app  : appDef  -> smt_context -> smt_sort list -> smt_ast list -> smt_ast
  val is_interp   : Ast.Sort.tycon -> bool
end

(* Theorem Prover API *)

(* RJ: This is CLEARLY the wrong API and in need of a major refactoring. *)

module type SMTSOLVER = sig
  
  type context
  type symbol 
  type ast
  type sort
  type fun_decl

  val var : context -> symbol -> sort -> ast 
  
  val var : Z3.context -> Z3.symbol -> Z3.sort -> Z3.ast
  val boundVar : Z3.context -> int -> Z3.sort -> Z3.ast
  val stringSymbol : Z3.context -> string -> Z3.symbol
  val funcDecl :
      Z3.context -> Z3.symbol -> Z3.sort array -> Z3.sort -> Z3.func_decl
  val isBool : Z3.context -> Z3.ast -> bool
  val smt_isInt : Z3.context -> Z3.ast -> bool
  val mkAll : Z3.context ->
              int ->
              Z3.pattern array ->
      Z3.sort array -> Z3.symbol array -> Z3.ast -> Z3.ast
    val mkEq : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkNe : Z3.context -> Z3.ast array -> Z3.ast
    val mkGt : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkGe : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkLt : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkLe : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkApp : Z3.context -> Z3.func_decl -> Z3.ast array -> Z3.ast
    val mkMul : Z3.context -> Z3.ast array -> Z3.ast
    val mkAdd : Z3.context -> Z3.ast array -> Z3.ast
    val mkSub : Z3.context -> Z3.ast array -> Z3.ast
    val mkMod : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkIte : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast -> Z3.ast
    val mkInt : Z3.context -> int -> Z3.sort -> Z3.ast
    val mkTrue : Z3.context -> Z3.ast
    val mkFalse : Z3.context -> Z3.ast
    val mkNot : Z3.context -> Z3.ast -> Z3.ast
    val mkAnd : Z3.context -> Z3.ast array -> Z3.ast
    val mkOr : Z3.context -> Z3.ast array -> Z3.ast
    val mkImp : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkIff : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val astString : Z3.context -> Z3.ast -> string
    val mkIntSort : Z3.context -> Z3.sort
    val mkBoolSort : Z3.context -> Z3.sort
    val mkSetSort : Z3.context -> Z3.sort -> Z3.sort
    val mkEmptySet : Z3.context -> Z3.sort -> Z3.ast
    val mkSetAdd : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkSetMem : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkSetCup : Z3.context -> Z3.ast array -> Z3.ast
    val mkSetCap : Z3.context -> Z3.ast array -> Z3.ast
    val mkSetDif : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkSetSub : Z3.context -> Z3.ast -> Z3.ast -> Z3.ast
    val mkContext : (string * string) array -> Z3.context
    val z3push : Z3.context -> unit
    val z3pop : Z3.context -> unit
    val unsat : Z3.context -> bool
    val assertAxiom : Z3.context -> Z3.ast -> unit
    val bracket : Z3.context -> (unit -> 'a) -> 'a
    val assertPreds : Z3.context -> Z3.ast list -> unit
    val valid : Z3.context -> Z3.ast -> bool
    val contra : Z3.context -> Z3.ast -> bool
    val print_stats : F.formatter -> unit -> unit

end

module type PROVER = 
sig
  type t 
 
  val create      : Ast.Sort.t list                         (* sorts        *) 
                    -> Ast.Sort.t Ast.Symbol.SMap.t         (* environment  *)
                    -> Ast.pred list                        (* axioms       *) 
                    -> Ast.Symbol.t list                    (* distinct constants, sorts in env *)
                    -> t
 
  val set_filter  : t 
                    -> Ast.Sort.t Ast.Symbol.SMap.t 
                    -> Ast.Symbol.t 
                    -> Ast.pred list 
                    -> ('a * Ast.pred) list 
                    -> 'a list

  val print_stats : Format.formatter -> t -> unit

  (*
  val unsat_cores : t                                       
                    -> Ast.Sort.t Ast.Symbol.SMap.t 
                    -> Ast.pred                             (* background predicate   *)
                    -> ('a * Ast.pred) list                 (* [(index, killer-fact)] *)
                    -> ('b * Ast.pred) list                 (* [(index, killed-fact)] *)
                    -> ('b * 'a list) list                  (* [(killed, killers)]    *)

  *)
 
  val is_contra   : t  
                    -> Ast.Sort.t Ast.Symbol.SMap.t 
                    -> Ast.pred
                    -> bool

  val unsat_core  : t                                       
                    -> Ast.Sort.t Ast.Symbol.SMap.t 
                    -> Ast.pred                             (* background predicate   *)
                    -> ('a * Ast.pred) list                 (* [(index, killer-fact)] *)
                    -> 'a list                              (* [unsat-core-index]    *)

  val unsat_suffix : t
                   -> Ast.Sort.t Ast.Symbol.SMap.t 
                   -> Ast.pred                              (* background predicate   *)
                   -> Ast.pred list                         (* [p0,...,pn] *)
                   -> int option                            (* max j st. p /\i=j..n pi unsat *)

end
