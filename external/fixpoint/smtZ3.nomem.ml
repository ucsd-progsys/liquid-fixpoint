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
(** DUMMY SMT-Z3 Solver (for non Z3MEM builds) **********************************)
(********************************************************************************)

let assertf = FixMisc.Ops.assertf
let msg     = "This build is NOT linked against Z3. Please rebuild with Z3MEM=true. Only possible on linux"

module SMTZ3 : ProverArch.SMTSOLVER = struct

type context     = ()
type symbol      = ()   
type sort        = () 
type ast         = () 
type fun_decl    = ()  

let var          _   = failwith msg 
let boundVar     _   = failwith msg
let stringSymbol _   = failwith msg
let funcDecl     _   = failwith msg
let isBool _         = failwith msg
let isInt _          = failwith msg
let mkAll _          = failwith msg
let mkRel _          = failwith msg
let mkApp _          = failwith msg  
let mkMul _          = failwith msg
let mkAdd _          = failwith msg
let mkSub _          = failwith msg
let mkMod _          = failwith msg
let mkIte _          = failwith msg
let mkInt _          = failwith msg  
let mkTrue _         = failwith msg
let mkFalse _        = failwith msg
let mkNot _          = failwith msg
let mkAnd _          = failwith msg 
let mkOr _           = failwith msg 
let mkImp _          = failwith msg 
let mkIff _          = failwith msg
let astString _      = failwith msg
let mkIntSort _      = failwith msg
let mkBoolSort _     = failwith msg
let mkSetSort _      = failwith msg
let mkEmptySet _     = failwith msg
let mkSetAdd _       = failwith msg
let mkSetMem _       = failwith msg
let mkSetCup _       = failwith msg
let mkSetCap _       = failwith msg
let mkSetDif _       = failwith msg
let mkSetSub _       = failwith msg
let mkContext _      = failwith msg
let unsat _          = failwith msg  
let assertAxiom _    = failwith msg
let assertDistinct _ = failwith msg
let bracket _        = failwith msg
let assertPreds _    = failwith msg
let valid _          = failwith msg 
let contra _         = failwith msg
let print_stats _    = failwith msg

end
