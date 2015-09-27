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
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATION.
 *
 *)

(* HIDE: all the sigs for defined binders

type rpred = A.pred
type vdef = Sy.t * So.t
type kdef = Sy.t * vdef list
type cstr = { lhs : A.pred; rhs : rpred option; id : int; }
type smtlib = {
  vars : vdef list;
  kvars : kdef list;
  cstrs : cstr list;
  consts : vdef list;
}
type kmap = kdef SM.t
val is_kvar : Sy.t -> bool
val print_brel : F.formatter -> A.brel -> unit
val print_bop : F.formatter -> A.bop -> unit
val print_pred : F.formatter -> A.pred -> unit
val print_expr_app : Sy.t -> F.formatter -> A.expr list -> unit
val print_expr : F.formatter -> A.expr -> unit
val print_sort_base : Format.formatter -> So.t -> unit
val print_sort : Format.formatter -> So.t -> unit
val print_fun_sorts : Format.formatter -> So.t list -> unit
val print_vdef : Format.formatter -> Sy.t * So.t -> unit
val print_const : Format.formatter -> Sy.t * So.t -> unit
val groupConsts : ('a * 'b) list -> ('b * 'a list) list
val print_distinct : Format.formatter -> So.t * Sy.t list -> unit
val print_kdef : Format.formatter -> Sy.t * ('a * So.t) list -> unit
val print_rhs : Format.formatter -> A.pred option -> unit
val binds_of_cstr : 'a SM.t -> cstr -> (SM.key * 'a) list
val print_cstr : So.t SM.t -> Format.formatter -> cstr -> unit
val print : Format.formatter -> smtlib -> unit
val sort_compat : 'a -> So.t -> So.t -> bool
val vdefs_of_env : C.envt -> C.reft -> (Ast.Symbol.t * Sort.t) list
val update_vmap : So.t SM.t -> Sy.t * So.t -> So.t SM.t
val update_vmap_int : So.t SM.t -> SM.key * 'a -> So.t SM.t
val add_c_var_to_vmap : So.t SM.t -> C.t -> So.t SM.t
val add_wf_var_to_vmap : So.t SM.t -> C.wf -> So.t SM.t
val check_no_subs : int -> (A.Subst.t * 'a) list -> unit
val join : (SM.key * So.t) list -> (SM.key * So.t) list -> (SM.key * So.t) list
val update_kmap : vdef list -> kdef SM.t -> SM.key -> kmap
val add_wf_to_kmap : kdef SM.t -> C.wf -> kdef SM.t
val make_kmap : Cg.deft list -> kmap
val mkFreshI : unit -> int
val mkFresh : int -> Sy.t -> Sy.t
val fresh_vars : 'a SM.t -> int -> A.expr list -> ((Sy.t * 'a) list * A.pred) * A.expr list
val pred_of_kdef : A.Symbol.t * (A.Symbol.t * 'a) list -> A.pred
val soln_of_kmap : (A.Symbol.t * (A.Symbol.t * 'a) list) SM.t -> SM.key -> A.pred list
val tx_constraint : C.soln -> C.t -> ('a list * cstr) list
val tx_defs : 'a Cg.cfg -> smtlib
val split_by_assertion : 'a Cg.cfg -> 'a Cg.cfg list
val slice_by_assertion : 'a Cg.cfg -> 'a Cg.cfg
val dump_smtlib_indexed : int option * 'a Cg.cfg -> unit
val dump_smtlib_mono : 'a Cg.cfg -> 'b
val dump_smtlib_sliced : 'a Cg.cfg -> 'b
val dump_smtlib : 'a Cg.cfg -> 'b

*)

(* val render : Format.formatter -> FixConfig.deft list -> unit
val render : Format.formatter -> 'a FixConfig.cfg -> unit
*)
val dump_smtlib : 'a FixConfig.cfg -> unit
