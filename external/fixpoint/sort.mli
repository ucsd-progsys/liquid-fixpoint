(*
 * Copyright � 2009 The Regents of the University of California. All rights reserved.
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
 *)

type tycon
type t
type sub

type loc =
  | Loc  of string
  | Lvar of int
  | LFun

val tycon       : string -> tycon

val tycon_string: tycon -> string

val to_string   : t -> string
val print       : Format.formatter -> t -> unit
val t_num       : t
val t_frac      : t
val t_obj       : t
val t_bool      : t
val t_int       : t
val t_real      : t
val t_generic   : int -> t
val t_ptr       : loc -> t
val t_func      : int -> t list -> t
val t_app_tc    : tycon -> t list -> t
val t_app       : t -> t list -> t
(* val t_fptr      : t *)

val is_bool      : t -> bool
val is_int       : t -> bool
val is_real      : t -> bool
val is_func      : t -> bool
val is_kind      : t -> bool
val ptr_of_t     : t -> loc option

val compat      : t -> t -> bool
val empty_sub   : sub
val unifyWith   : sub -> t list -> t list -> sub option
val unify       : t list -> t list -> sub option
val apply       : sub -> t -> t
val generalize  : t list -> t list
val sub_args    : sub -> (int * t) list
    (* val check_arity : int -> sub -> bool *)
val makeFresh : int -> (int * int) list
val refresh   : (int * int) list -> t -> t

val mgu : int -> t -> t -> (int * t) list
val apply_ty : (int * t) list -> t -> t
