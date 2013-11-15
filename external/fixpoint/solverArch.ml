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

type qbind   = Qualifier.t list

module type DOMAIN = sig
  type t
  type bind
  val empty        : t 
  (* val meet         : t -> t -> t *)
  val is_bot	   : bind -> bool
  val min_read     : t -> FixConstraint.soln
  val read         : t -> FixConstraint.soln
  val read_bind    : t -> Ast.Symbol.t -> bind
  val top          : t -> Ast.Symbol.t list -> t
  val refine       : t -> FixConstraint.t -> (bool * t)
  val unsat        : t -> FixConstraint.t -> bool
  val create       : bind FixConfig.cfg -> FixConstraint.soln option -> t
  val print        : Format.formatter -> t -> unit
  val print_stats  : Format.formatter -> t -> unit
  val dump         : t -> unit
  val simplify     : t -> t
  val ctr_examples : t -> FixConstraint.t list -> FixConstraint.t list -> Counterexample.cex list 
  val mkbind       : qbind -> bind
end
