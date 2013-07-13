(*
 * Copyright © 2008-13 The Regents of the University of California. All rights reserved.
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

module H    = Hashtbl
module F    = Format
module Co   = Constants
module BS   = BNstats
module A    = Ast
module Sy   = A.Symbol
module So   = A.Sort
module SM   = Sy.SMap
module P    = A.Predicate
module E    = A.Expression
module Misc = FixMisc open Misc.Ops
module SSM  = Misc.StringMap
module Th   = Theories

module Prover : ProverArch.PROVER = struct

type t = int  
 
(**************************************************************************)
(**************************** API *****************************************)
(**************************************************************************)

let create (sorts: So.t list) (env: So.t SM.t) (axioms: Ast.pred list) (consts: Sy.t list) : t
  = failwith "TBD"

let set_filter (me: t) (env: S.t SM.t) (vv: Sy.t) (hyps: Ast.pred list) (goals: ('a * Ast.pred) list) : 'a list
  = failwith "TBD"

let print_stats (pp: Format.formatter) (me:t) : unit
  = failwith "TBD"

let is_contra (me : t) (env : So.t SM.t) (goal: Ast.pred) : bool
  = failwith "TBD"

end

