(* This module deals with top-level parsing of fq files and such *)

(*
exception UnmappedKvar of Ast.Symbol.t
*)
type solbind = Ast.Symbol.t * ((Ast.Symbol.t * (Ast.expr list)) list)

type deft = Srt of Sort.t
          | Axm of Ast.pred
          | Cst of FixConstraint.t
          | Wfc of FixConstraint.wf
          | Con of Ast.Symbol.t * Sort.t
          | Sol of solbind
          | Qul of Qualifier.t
          | Dep of FixConstraint.dep
          | Kut of Ast.Symbol.t
          | IBind of int * Ast.Symbol.t * FixConstraint.reft

type 'bind cfg = {
   a     : int                               (* Tag arity                            *)
 ; ts    : Sort.t list                   (* New sorts, now = []                  *)
 ; ps    : Ast.pred list                     (* New axioms, now = []                 *)
 ; cs    : FixConstraint.t list              (* Implication Constraints              *)
 ; ws    : FixConstraint.wf list             (* Well-formedness Constraints          *)
 ; ds    : FixConstraint.dep list            (* Constraint Dependencies              *)
 ; qs    : Qualifier.t list                  (* Qualifiers                           *)
 ; kuts  : Ast.Symbol.t list                 (* "Cut"-Kvars, which break cycles      *)
 ; bm    : 'bind Ast.Symbol.SMap.t           (* Initial Sol Bindings                 *)
 ; uops  : Sort.t Ast.Symbol.SMap.t      (* Globals: measures + distinct consts) *)
 ; cons  : Ast.Symbol.t list                 (* Distinct Constants, defined in uops  *)
 ; assm  : FixConstraint.soln                (* Seed Solution -- must be a fixpoint over constraints *)
}




module type SIMPLIFIER = sig
  val simplify_ts: FixConstraint.t list -> FixConstraint.t list
end

val empty     : 'a cfg
val create    : deft list -> (Qualifier.t list) cfg
val print     : Format.formatter -> 'a cfg -> unit
val create_raw:  Sort.t list
              -> Sort.t Ast.Symbol.SMap.t 
              -> Ast.pred list
              -> int
              -> FixConstraint.dep list
              -> FixConstraint.t list
              -> FixConstraint.wf list
              -> Qualifier.t list
              -> Ast.Symbol.t list
              -> FixConstraint.soln
              -> 'a cfg
