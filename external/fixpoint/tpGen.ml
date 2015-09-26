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

module H  = Hashtbl
module F  = Format
module Co = Constants
module BS = BNstats
module A  = Ast
module Sy = A.Symbol
module So = A.Sort
module SM   = Sy.SMap
module P    = A.Predicate
module E    = A.Expression
module Misc = FixMisc
module SSM  = Misc.StringMap
module SMT  = SmtZ3.SMTZ3

open Misc.Ops
open ProverArch

let mydebug = false

module MakeProver(SMT : SMTSOLVER) : PROVER = struct

  module Th   = Theories.MakeTheory(SMT)

(*************************************************************************)
(*************************** Type Definitions ****************************)
(*************************************************************************)

type decl    = Vbl of (Sy.t * So.t) | Fun of Sy.t * int | Barrier

type var_ast = Const of SMT.ast | Bound of int * So.t

type t = {
  c             : SMT.context;
  tint          : SMT.sort;
  treal         : SMT.sort;
  tbool         : SMT.sort;
  vart          : (decl, var_ast) H.t;
  funt          : (decl, SMT.fun_decl) H.t;
  tydt          : (So.t, SMT.sort) H.t;
  mutable vars  : decl list ;
  mutable bnd   : int;
  thy_sortm     : (So.tycon, Th.sortDef) H.t;
  thy_symm      : (Sy.t,     Th.appDef)  H.t;
}

(*************************************************************************)
(************************** Pretty Printing ******************************)
(*************************************************************************)

let pprint_decl ppf = function
  | Vbl (x, t) 	-> F.fprintf ppf "%a:%a" Sy.print x So.print t
  | Barrier 	-> F.fprintf ppf "----@."
  | Fun (s, i) 	-> F.fprintf ppf "%a[%i]" Sy.print s i

let dump_decls me =
  F.printf "Vars: %a" (Misc.pprint_many true "," pprint_decl) me.vars

(************************************************************************)
(***************************** Stats Counters  **************************)
(************************************************************************)

let nb_set  		= ref 0
let nb_query 		= ref 0

(************************************************************************)
(********************** Misc. Constants *********************************)
(************************************************************************)

let div_n  = Sy.of_string "_DIV"
let tag_n  = Sy.of_string "_TAG"
let mul_n  = Sy.of_string "_MUL"

let axioms = []

(* TBD these are related to ML and should be in DSOLVE, not here *)
let builtins =
  SM.empty
  |> SM.add tag_n  (So.t_func 0 [So.t_obj; So.t_int])
  |> SM.add div_n  (So.t_func 0 [So.t_int; So.t_int; So.t_int])
  |> SM.add mul_n  (So.t_func 0 [So.t_int; So.t_int; So.t_int])

let select_t = So.t_func 0 [So.t_int; So.t_int]

let mk_select, is_select =
  let ss = "SELECT" in
  (fun f -> Sy.to_string f |> (^) (ss ^ "_") |> Sy.of_string),
  (fun s -> Sy.to_string s |> Misc.is_prefix ss)

let fresh =
  let x = ref 0 in
  fun v -> incr x; (v^(string_of_int !x))

(*************************************************************************)
(********************** Typing *******************************************)
(*************************************************************************)

let varSort env s =
  try SM.find s env with Not_found ->
    failure "ERROR: varSort cannot type %s in TPZ3 \n" (Sy.to_string s)

let funSort env s =
  try SM.find s builtins with Not_found ->
    try SM.find s env with Not_found ->
      if is_select s then select_t else
        failure "ERROR: could not type function %s in TPZ3 \n" (Sy.to_string s)

let rec z3Type me t =
  Misc.do_memo me.tydt begin fun t ->
    if So.is_bool t then me.tbool else
      if So.is_int t then me.tint else
        if So.is_real t then me.treal else
          Misc.maybe_default (z3TypeThy me t) me.tint
  end t t

and z3TypeThy me t =
  match So.app_of_t t with
  | Some (c, ts) when H.mem me.thy_sortm c ->
     let def = H.find me.thy_sortm c   in
     let zts = List.map (z3Type me) ts in
     Some (Th.mk_thy_sort def me.c zts)
  | _ ->
     None

(***********************************************************************)
(********************** Identifiers ************************************)
(***********************************************************************)

let getVbl env x = Vbl (x, varSort env x)

let z3Var_memo me env x =
  let vx  = getVbl env x in
  Misc.do_memo me.vart
    (fun () ->
      let t   = x |> varSort env |> z3Type me in
      let sym = fresh "z3v"
             (* >> F.printf "z3Var_memo: %a :->  %s\n" Sy.print x *)
                |> SMT.stringSymbol me.c in
      let rv  = Const (SMT.var me.c sym t) in
      let _   = me.vars <- vx :: me.vars in
      rv)
    () vx

let z3Var me env x =
  match BS.time "z3Var memo" (z3Var_memo me env) x with
  | Const v      -> v
  | Bound (b, t) -> SMT.boundVar me.c (me.bnd - b) (z3Type me t)


let z3Fun me env p t k =
  Misc.do_memo me.funt begin fun _ ->
    match So.func_of_t t with
    | None             -> assertf "MATCH ERROR: z3ArgTypes"
    | Some (_, ts, rt) ->
        let ts = List.map (z3Type me) ts in
        let rt = z3Type me rt in
        let cf = SMT.stringSymbol me.c (fresh "z3f") in
        let rv = SMT.funcDecl me.c cf (Array.of_list ts) rt in
        let _  = me.vars <- (Fun (p,k))::me.vars in
        rv
  end () (Fun (p, k))

(**********************************************************************)
(********************** Pred/Expr Transl ******************************)
(**********************************************************************)

let opt_to_string p = function
  | None   -> "none"
  | Some x -> p x

exception Z3RelTypeError

let pred_sort env p =
  A.sortcheck_pred Theories.is_interp (Misc.flip SM.maybe_find env) p

let app_sort env tyo f es =
  A.sortcheck_app Theories.is_interp (Misc.flip SM.maybe_find env) tyo f es
(*  >> (fun so -> let _ = F.printf "app_sort [f = %a es = %a] = %a\n"
                         Sy.print f
                         (Misc.pprint_many false ", " E.print) es
                         (Misc.pprint_maybe So.print) (Misc.maybe_map snd so)
                in
                ()
     )
 *)

let expr_sort env = function
  | A.App (f, es), _ -> Misc.maybe_map snd <| app_sort env None f es
  | e                -> A.sortcheck_expr Theories.is_interp (Misc.flip SM.maybe_find env) e


let z3Bind me env x t =
  let vx = Vbl (x, varSort env x) in
  me.bnd <- me.bnd + 1;
  H.replace me.vart vx (Bound (me.bnd, t));
  me.vars <- vx :: me.vars;
  SMT.stringSymbol me.c (fresh "z3b")

let rec z3Rel me env (e1, r, e2) =
  let p  = A.pAtom (e1, r, e2) in
  let ok = pred_sort env p     in
  (* let _  = F.printf "z3Rel: e = %a, res = %b \n" P.print p ok in
     let _  = F.print_flush ()                                   in *)

  if ok then
    z3Rel_cast me env (e1, r, e2)
    (* z3Rel_real me env (e1, r, e2) *)
    (* SMT.mkRel me.c r (z3Exp me env e1) (z3Exp me env e2) *)
  else begin
    SM.iter (fun s t -> F.printf "@[%a :: %a@]@." Sy.print s So.print t) env;
    F.printf "@[%a@]@.@." P.print (A.pAtom (e1, r, e2));
    F.print_flush ();
    raise Z3RelTypeError
  end




and z3Rel_cast me env = function
  | (e1, A.Eq, e2) -> begin
      let (t1o, t2o) = (expr_sort env e1, expr_sort env e2) in
      (*
      let _ = F.printf "z3Rel_cast: t1o = %s, t2o = %s \n"
                       (opt_to_string So.to_string t1o)
                       (opt_to_string So.to_string t2o) in
       *)
      match (t1o, t2o) with
        | (Some t , None  ) -> z3Rel_real me env (e1, A.Eq, A.eCst (e2, t))
        | (None   , Some t) -> z3Rel_real me env (A.eCst (e1, t), A.Eq, e2)
        | (_      , _     ) -> z3Rel_real me env (e1, A.Eq, e2)
    end
  | (e1, r, e2) ->
      z3Rel_real me env (e1, r, e2)

and z3Rel_real me env (e1, r, e2) =
  (* let _ = F.printf "z3Rel_real: e1 = %s, e2 = %s \n"
            (E.to_string e1)
            (E.to_string e2) in
   *)
  SMT.mkRel me.c r (z3Exp me env e1) (z3Exp me env e2)

and z3App me env p zes =
  let t  = funSort env p                      in
  let cf = z3Fun me env p t (List.length zes) in
  SMT.mkApp me.c cf zes

and z3AppThy me env def tyo f es =
  (* match A.sortcheck_app Theories.is_interp (Misc.flip SM.maybe_find env) tyo f es with *)
  match app_sort env tyo f es with
    | Some (s, t) ->
        let zts = So.sub_args s |> List.map (snd <+> z3Type me) in
        let zes = es            |> List.map (z3Exp me env)      in
        Th.mk_thy_app def me.c zts zes
    | None ->
        A.eApp (f, es)
        |> E.to_string
        |> assertf "z3AppThy: sort error %s"

and z3Div me env = function
  | (e1, e2) when !Co.uif_divide ->
     z3App me env div_n (List.map (z3Exp me env) [e1; e2])
  | (e1, e2) ->
     SMT.mkDiv me.c (z3Exp me env e1) (z3Exp me env e2)

and z3Mul me env = function
  | ((A.Con (A.Constant.Int i), _), e)
  | (e, (A.Con (A.Constant.Int i), _)) ->
      SMT.mkMul me.c (SMT.mkInt me.c i me.tint) (z3Exp me env e)
  | (e1, e2) when !Co.uif_multiply ->
      z3App me env mul_n (List.map (z3Exp me env) [e1; e2])
  | (e1, e2) ->
      SMT.mkMul me.c (z3Exp me env e1) (z3Exp me env e2)

and z3Con me env = function
  | A.Constant.Int i ->
      SMT.mkInt me.c i me.tint
  | A.Constant.Real i ->
      SMT.mkReal me.c i me.treal
  | A.Constant.Lit (l, t) ->
      SMT.mkLit me.c l (z3Type me t)

and z3Exp me env = function
  | A.Con c, _ ->
      z3Con me env c
  | A.Var s, _ ->
      z3Var me env s
  | A.Cst ((A.App (f, es), _), t), _ when (H.mem me.thy_symm f) ->
      z3AppThy me env (H.find me.thy_symm f) (Some t) f es
  | A.App (f, es), _ when (H.mem me.thy_symm f) ->
      z3AppThy me env (H.find me.thy_symm f) None f es
  | A.App (f, es), _  ->
      z3App me env f (List.map (z3Exp me env) es)
  | A.Bin (e1, A.Plus, e2), _ ->
      SMT.mkAdd me.c (z3Exp me env e1) (z3Exp me env e2)
  | A.Bin (e1, A.Minus, e2), _ ->
      SMT.mkSub me.c (z3Exp me env e1) (z3Exp me env e2)
  | A.Bin((A.Con (A.Constant.Int n1), _), A.Times, (A.Con (A.Constant.Int n2), _)),_ ->
      SMT.mkInt me.c (n1 * n2) me.tint
  | A.Bin (e1, A.Times, e2), _ ->
      z3Mul me env (e1, e2)
  | A.Bin (e1, A.Div, e2), _ ->
      z3Div me env (e1, e2)
  | A.Bin (e, A.Mod, (A.Con (A.Constant.Int i), _)), _ ->
      SMT.mkMod me.c (z3Exp me env e) (SMT.mkInt me.c i me.tint)
  | A.Bin (e1, A.Mod, e2), _ ->
      SMT.mkMod me.c (z3Exp me env e1) (z3Exp me env e2)
  | A.Ite (e1, e2, e3), _ ->
      SMT.mkIte me.c (z3Pred me env e1) (z3Exp me env e2) (z3Exp me env e3)

  | A.Fld (f, e), _ ->
      z3App me env (mk_select f) [z3Exp me env e] (** REQUIRES: disjoint field names *)
  | A.Cst (e, _), _ ->
      z3Exp me env e
  | e ->
      assertf "z3Exp: Cannot Convert %s!" (E.to_string e)

and z3Pred me env = function
  | A.True, _ ->
      SMT.mkTrue  me.c
  | A.False, _ ->
      SMT.mkFalse me.c
  | A.Not p, _ ->
      SMT.mkNot  me.c (z3Pred me env p)
  | A.And ps, _ ->
      SMT.mkAnd me.c (List.map (z3Pred me env) ps)
  | A.Or ps, _  ->
      SMT.mkOr  me.c (List.map (z3Pred me env) ps)
  | A.Imp (p1, p2), _ ->
      SMT.mkImp me.c (z3Pred me env p1) (z3Pred me env p2)
  | A.Iff (p1, p2), _ ->
      SMT.mkIff me.c (z3Pred me env p1) (z3Pred me env p2)
  | A.Atom (e1, r, e2), _ ->
      z3Rel me env (e1, r, e2)
  | A.Bexp e, _ ->
      let a  = z3Exp me env e in
      let s2  = E.to_string e in
      let so = match expr_sort env e with
                 | Some so -> so
                 | _       -> let _ = F.printf "No type for %s" (E.to_string e) in assert false
      in
      let sos = So.to_string so in
      (* let s1  = SMT.astString me.c a in
      let _   = asserts (SMT.isBool me.c a)
                        "Bexp is not bool (e = %s)! z3=%s, fix=%s, sort=%s"
                        (E.to_string e) s1 s2 sos
      in *)
      a

  | A.Forall (xts, p), _ ->
      let (xs, ts) = List.split xts                                  in
      let zargs    = Array.of_list (List.map2 (z3Bind me env) xs ts) in
      let zts      = Array.of_list (List.map  (z3Type me) ts)        in
      let rv       = SMT.mkAll me.c zts zargs (z3Pred me env p)      in
      let _        = me.bnd <- me.bnd - (List.length xs)             in
      rv
  | p ->
      assertf "z3Pred: Cannot Convert %s!" (P.to_string p)



let z3Pred me env p =
  try
    let p = p in (*BS.time "fixdiv" A.fixdiv p in *)
    BS.time "z3Pred" (z3Pred me env) p
  with ex -> (F.printf "z3Pred: error converting %a\n" P.print p) ; raise ex

(***************************************************************************)
(***************** Binder/Stack Management *********************************)
(***************************************************************************)

let rec vpop (cs,s) =
  match s with
  | []           -> (cs,s)
  | Barrier :: t -> (cs,t)
  | h :: t       -> vpop (h::cs,t)

let clean_decls me =
  let cs, vars' = vpop ([],me.vars) in
  let _         = me.vars <- vars'  in
  List.iter begin function
    | Barrier    -> failure "ERROR: TPZ3-cleanDecls"
    | Vbl _ as d -> H.remove me.vart d
    | Fun _ as d -> H.remove me.funt d
  end cs

let handle_vv me env vv =
  H.remove me.vart (getVbl env vv) (* RJ: why are we removing this? *)


(************************************************************************)
(********************************* API **********************************)
(************************************************************************)

let create_theories () =
  Th.theories ()
  |> (Misc.hashtbl_of_list_with Th.sort_name <**> Misc.hashtbl_of_list_with Th.sym_name)

let assert_distinct_constants me env = function [] -> () | cs ->
  cs |> Misc.kgroupby (varSort env)
     |> List.iter begin fun (_, xs) ->
         xs >> Co.bprintf mydebug "Distinct Constants: %a \n" (Misc.pprint_many false ", " Sy.print)
            |> List.map (z3Var me env)
            |> SMT.assertDistinct me.c
         end

let prep_preds me env ps =
  let ps = List.rev_map (z3Pred me env) ps in
  let _  = me.vars <- Barrier :: me.vars in
  ps

let valid me p =
  SMT.bracket me begin fun _ ->
    SMT.assertPreds me [SMT.mkNot me p];
    BS.time "unsat" SMT.unsat me
  end

(* API *)
let set_filter (me: t) (env: So.t SM.t) (vv: Sy.t) ps qs =
  let _   = ignore(nb_set   += 1); ignore (nb_query += List.length qs) in
  let _   = handle_vv me env vv  in
  let zps = prep_preds me env ps in (* DO NOT PUSH INSIDE SMT.bracket or z3 blocks postests/ll3.c *)
  SMT.bracket me.c begin fun _ ->
    let _        = SMT.assertPreds me.c zps                      in
    let tqs, fqs = List.partition (snd <+> P.is_tauto) qs        in
    let fqs      = fqs |> List.rev_map (Misc.app_snd (z3Pred me env))
                       |> Misc.filter  (snd <+> valid me.c)      in
    let _        = clean_decls me                                in
    (List.map fst tqs) ++ (List.map fst fqs)
  end

(* API *)
let print_stats ppf me =
  SMT.print_stats ppf ();
  F.fprintf ppf "TP stats: sets=%d, queries=%d, count=%d\n"
    !nb_set !nb_query (List.length me.vars)

(*************************************************************************)
(****************** Unsat Core for CEX generation ************************)
(*************************************************************************)

let mk_prop_var me pfx i : SMT.ast =
  i |> string_of_int
    |> (^) pfx
    |> SMT.stringSymbol me.c
    |> Misc.flip (SMT.var me.c) me.tbool

let mk_prop_var_idx me ipa : (SMT.ast array * (SMT.ast -> 'a option)) =
  let va  = Array.mapi (fun i _ -> mk_prop_var me "uc_p_" i) ipa in
  let vm  = va
            |> Array.map   (SMT.astString me.c)
            |> Misc.array_to_index_list
            |> List .map Misc.swap
            |> SSM.of_list in
  let f z = SSM.maybe_find (SMT.astString me.c z) vm
            |> Misc.maybe_map (fst <.> Array.get ipa) in
  (va, f)

let mk_pa me p2z pfx ics =
  ics |> List.map (Misc.app_snd p2z)
      |> Array.of_list
      |> Array.mapi (fun i (x, p) -> (x, p, mk_prop_var me pfx i))

(* API *)
let unsat_core me env bgp ips =
  let _     = H.clear me.vart                                       in
  let p2z   = (*A.fixdiv <+>*) z3Pred me env                            in
  let ipa   = ips |> List.map (Misc.app_snd p2z) |> Array.of_list   in
  let va, f = mk_prop_var_idx me ipa                                in
  let zp    = ipa |> Array.mapi (fun i (_, p) -> SMT.mkIff me.c va.(i) p)
                  |> Array.to_list
                  |> (++) [p2z bgp]
                  |> SMT.mkAnd me.c in
  SMT.bracket me.c begin fun _ ->
    let _   = SMT.assertPreds me.c [zp] in
    let n   = Array.length va in
    let va' = Array.map id va in
    failwith "SMT-UNSAT-CORE-TODO"
    (************************************
    match SMT.check_assumptions me.c va n va' with
      | (Z3.L_FALSE, m,_, n, ucore)
          -> Array.map f ucore |> Array.to_list |> Misc.map_partial id
      | _ -> []
      ************************************)
  end

let contra me p =
  SMT.bracket me begin fun _ ->
    SMT.assertPreds me [p];
    BS.time "unsat" SMT.unsat me
  end

(* API *)
let is_contra me env =  z3Pred me env <+> contra me.c

(* API *)
let unsat_suffix me env p ps =
  let _ = if SMT.unsat me.c then assertf "ERROR: unsat_suffix" in
  SMT.bracket me.c begin fun _ ->
    let rec loop j = function [] -> None | zp' :: zps' ->
      SMT.assertPreds me.c [zp'];
      if SMT.unsat me.c then Some j else loop (j-1) zps'
    in loop (List.length ps) (List.map (z3Pred me env) (p :: List.rev ps))
  end

(***********************************************************************)
(******** Prover Object ************************************************)
(***********************************************************************)

(* API *)
let create ts env ps consts =
  let _        = asserts (ts = []) "ERROR: TPGEN-create non-empty sorts!" in
  let c        = SMT.mkContext [|("MODEL", "false"); ("MODEL_PARTIAL", "true")|] in
  let som, sym = create_theories () in
  let me       = { c     = c;
                   tint  = SMT.mkIntSort  c;
                   treal = SMT.mkRealSort c;
                   tbool = SMT.mkBoolSort c;
                   tydt  = H.create 37;
                   vart  = H.create 37;
                   funt  = H.create 37;
                   vars  = [];
                   bnd   = 0;
                   thy_sortm = som;
                   thy_symm  = sym
                 }
  in
  let _  = List.iter (z3Pred me env <+> SMT.assertAxiom me.c) (axioms ++ ps) in
  let _  = assert_distinct_constants me env consts                      in
  me

class tprover ts env ps consts : prover =
  object (self)
    val me = create ts env ps consts
    method interp_syms  = Theories.interp_syms
    method set_filter :  'a. Sort.t Symbol.SMap.t
                          -> Symbol.t
                          -> Ast.pred list
                          -> ('a * Ast.pred) list
                          -> 'a list
                        = set_filter me
    method print_stats  = fun ppf -> print_stats ppf me
    method is_contra    = is_contra me
    method unsat_suffix = unsat_suffix me
  end

let mkProver ts env ps consts = new tprover ts env ps consts

end
