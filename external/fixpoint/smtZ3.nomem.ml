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

let var          _   = assertf msg 
let boundVar     _   = assertf msg
let stringSymbol _   = assertf msg
let funcDecl     _   = assertf msg
let isBool c a       = assertf msg
let isInt me a       = assertf msg
let mkAll me         = assertf msg
let mkApp _          = assertf msg  
let mkMul _          = assertf msg
let mkAdd _          = assertf msg
let mkSub _          = assertf msg
let mkMod _          = assertf msg
let mkIte _          = assertf msg
let mkInt _          = assertf msg  
let mkTrue _         = assertf msg
let mkFalse _        = assertf msg
let mkNot _          = assertf msg
let mkAnd _          = assertf msg 
let mkOr _           = assertf msg 
let mkImp _          = assertf msg 
let mkIff _          = assertf msg
let astString _      = assertf msg
let mkIntSort _      = assertf msg
let mkBoolSort _     = assertf msg
let mkSetSort _      = assertf msg
let mkEmptySet _     = assertf msg
let mkSetAdd _       = assertf msg
let mkSetMem _       = assertf msg
let mkSetCup _       = assertf msg
let mkSetCap _       = assertf msg
let mkSetDif _       = assertf msg
let mkSetSub _       = assertf msg
let mkContext _      = assertf msg
let unsat _          = assertf msg  
let assertAxiom _    = assertf msg
let assertDistinct _ = assertf msg
let bracket _        = assertf msg
let assertPreds _    = assertf msg
let valid _          = assertf msg 
let contra _         = assertf msg
let print_stats _    = assertf msg

end
