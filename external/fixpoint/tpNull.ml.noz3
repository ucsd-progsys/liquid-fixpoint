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

(* USE THIS ON NON-Z3 PLATFORMS e.g. Mac *) 
module Prover : ProverArch.PROVER = struct
  type t                   = unit
  let create _ _ _ _       = () 
  let set_filter _ _ _ _ _ = failwith "TBD: TpNull.set_filter"
  let print_stats _ _      = failwith "TBD: TpNull.print_stats"
  let unsat_suffix _       = failwith "TBD: TpNull.unsat_suffix" 
  let unsat_core _         = failwith "TBD: TpNull.unsat_core"
  let is_contra   _        = failwith "TBD: TpNull.is_contra"  
  let interp_syms _        = failwith "TBD: TpNull.interp_syms" 
  let is_interp _          = failwith "TBD: TpNull.is_interp" 

end
