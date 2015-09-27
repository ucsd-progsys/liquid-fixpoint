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
 *)


(*************************************************************)
(******************** Solution Management ********************)
(*************************************************************)

module F   = Format
module A   = Ast
module E   = A.Expression
module P   = A.Predicate

module Q   = Qualifier
module QS  = Q.QSet
module Sy  = Symbol
module Su  = A.Subst
module SM  = Sy.SMap
module SS  = Sy.SSet
module C   = FixConstraint

module BS  = BNstats
module Co  = Constants
module Cg  = FixConfig
module H   = Hashtbl
module PH  = A.Predicate.Hash

module CX   = Counterexample
module Misc = FixMisc
module IM   = Misc.IntMap
module IS   = Misc.IntSet
open Misc.Ops

let mydebug = false

module Q2S = Misc.ESet (struct
  type t = Sy.t * Sy.t
  let compare x y = compare x y
end)

module V : Graph.Sig.COMPARABLE with type t = Sy.t = struct
  type t = Sy.t
  let hash    = Sy.to_string <+> H.hash
  let compare = compare
  let equal   = (=)
end



(*
let tag_of_qual2 = Misc.map_pair tag_of_qual

module Q2S = Misc.ESet (struct
  type t = Q.t * Q.t
  let compare x y = compare (tag_of_qual2 x) (tag_of_qual2 y)
end)

module V : Graph.Sig.COMPARABLE with type t = Q.t = struct
  type t = Q.t
  let hash    = tag_of_qual <+> H.hash
  let compare = fun q1 q2 -> compare (tag_of_qual q1) (tag_of_qual q2)
  let equal   = fun q1 q2 -> tag_of_qual q1 = tag_of_qual q2
end

*)

module Id : Graph.Sig.ORDERED_TYPE_DFT with type t = unit = struct
  type t = unit
  let default = ()
  let compare = compare
end

module G   = Graph.Persistent.Digraph.ConcreteLabeled(V)(Id)

module SCC = Graph.Components.Make(G)

type bind  = Bot | NonBot of Q.t list

(* API *)
let meet_bind b1 b2 = match (b1, b2) with
  | (Bot, _)              -> b2
  | (_, Bot)              -> b1
  | (NonBot x, NonBot y)  -> NonBot (x ++ y)

type t     =
  { tpc    : ProverArch.prover
  ; m      : bind SM.t
  ; om     : (Q.t list) SM.t
  ; wm     : (Sort.t SM.t * Symbol.t * Sort.t) SM.t
  ; assm   : FixConstraint.soln  (* invariant assumption for K, must be a fixpoint wrt constraints *)
  (* ; qm     : Q.t SM.t  *)     (* map from names to qualifiers *)
  ; qs     : Q.t list            (* list of qualifiers *)
  ; qleqs  : Q2S.t               (* (q1,q2) \in qleqs implies q1 => q2 *)
  ; seen   : IS.t                (* constraint (ids) that have been "refined" once *)
  (* counterexamples *)
  ; step     : CX.step         (* which iteration *)
  ; ctrace   : CX.ctrace
  ; lifespan : CX.lifespan

  (* stats *)
  ; stat_simple_refines : int ref
  ; stat_tp_refines     : int ref
  ; stat_imp_queries    : int ref
  ; stat_valid_queries  : int ref
  ; stat_matches        : int ref
  ; stat_umatches       : int ref
  ; stat_unsatLHS       : int ref
  ; stat_emptyRHS       : int ref

}

let lookup_bind k m = SM.find_default Bot k m

let pprint_ps =
  Misc.pprint_many false ";" P.print

let pprint_dep ppf q =
  F.fprintf ppf "(%a, %a)" P.print (Q.pred_of_t q) Q.print_args q

let pprint_ds =
  Misc.pprint_many false ";" pprint_dep

let pprint_bind ppf = function
  | Bot       -> F.fprintf ppf "(false, BOT())"
  | NonBot qs -> pprint_ds ppf qs

let pprint_qs ppf =
  F.fprintf ppf "[%a]" (Misc.pprint_many false ";" Q.print)

let pprint_qs' ppf =
  List.map (fst <+> snd <+> snd <+> fst) <+> pprint_qs ppf


(*************************************************************)
(************* Breadcrumbs for Cex Generation ****************)
(*************************************************************)

let cx_iter c me =
  { me with step = me.step + 1 }

let cx_ctrace b c me =
  (* let _ = if mydebug then F.printf "\nPredAbs.refine iter = %d cid = %d b = %b\n"
                          me.step (C.id_of_t c) b in
                          *)
  if b then { me with ctrace = IM.adds (C.id_of_t c) [me.step] me.ctrace } else me


let lookup_qualifiers k m =
  lookup_bind k m
  |> function | Bot       -> []
              | NonBot qs -> qs


let cx_update ks kqsm' me : t =
  List.fold_left begin fun me k ->
    let qs    = QS.of_list  (lookup_qualifiers k me.m)  in
    let qs'   = QS.of_list  (SM.finds k kqsm') in
    let kills = QS.elements (QS.diff qs qs')   in
    if Misc.nonnull kills
    then {me with lifespan = SM.adds k [(me.step, kills)] me.lifespan}
    else me
  end me ks

(*************************************************************)
(************* Constructing Initial Solution *****************)
(*************************************************************)

(*

let def_of_pred_qual (p, q) =
  let qp = Q.pred_of_t q in
  match A.unify_pred qp p with
  | Some su -> (p, q, su)
  | None    -> assertf "ERROR: unify q=%s p=%s" (P.to_string qp) (P.to_string p)

let map_of_bindings bs =
  List.fold_left begin fun s (k, ds) ->
    ds |> List.map Misc.single
       |> Misc.flip (SM.add k) s
  end SM.empty bs
*)

let quals_of_bindings bm =
  bm |> SM.range
     |> Misc.flatten
     (* |> Misc.map (snd <+> fst)  *)
     |> Misc.sort_and_compact
     >> (fun qs -> Co.bprintf mydebug "Quals of Bindings: \n%a" (Misc.pprint_many true "\n" Q.print) qs; flush stdout)

(************************************************************************)
(*************************** Dumping to Dot *****************************)
(************************************************************************)

module DotGraph = struct
  type t = G.t
  module V = G.V
  module E = G.E
  let iter_vertex               = G.iter_vertex
  let iter_edges_e              = G.iter_edges_e
  let graph_attributes          = fun _ -> [`Size (11.0, 8.5); `Ratio (`Fill (* Float 1.29*))]
  let default_vertex_attributes = fun _ -> [`Shape `Box]
  let vertex_name               = Sy.to_string
  let vertex_attributes         = fun q -> [`Label (Sy.to_string q)]
  let default_edge_attributes   = fun _ -> []
  let edge_attributes           = fun (_,(),_) -> []
  let get_subgraph              = fun _ -> None
end

module Dot = Graph.Graphviz.Dot(DotGraph)

let dump_graph s g =
  s |> open_out
    >> (fun oc -> Dot.output_graph oc g)
    |> close_out



(* INV: qs' \subseteq qs *)
let update m k ds' =
  let n' = List.length ds'                    in
  let n  = match SM.find_default Bot k m with
             | Bot        -> 1 + n'
             | NonBot qs  -> List.length qs   in
  let _  = asserts (n = 0 || n' <= n) "PredAbs.update: Non-monotone k = %s |ds| = %d |ds'| = %d \n" (Sy.to_string k) n n' in
  ((n != n'), SM.add k (NonBot ds') m)
  (* >> begin fun _ ->
        if n' > n && n > 0 then
          Co.bprintflush mydebug  <| Printf.sprintf "OMFG: update k = %s |ds| = %d |ds'| = %d \n"
                          (Sy.to_string k) n n'
     end
   *)

(* We must ensure there are NO duplicate k-q pairs in the update list.
 * If there are duplicate KVars in the ks then the kqs must be grouped:
 * a "k-q" binding is ONLY preserved  IF #bindings = #copies-of-k
 * If there are NO duplicate KVars there SHOULD BE no duplicate k-q pairs. *)

let group_ks_kqs ks kqs =
  if (Misc.cardinality ks = List.length ks) then
    kqs (* no duplicate kvars *)
  else
    let km = SM.frequency ks in
    kqs |> Misc.frequency
        |> Misc.filter (fun ((k, _), n) -> n = SM.find_default 0 k km)
        |> Misc.map fst

let p_update me ks kqs =
  (* let _    = Co.bprintflush mydebug (Printf.sprintf "p_update A: |kqs| = %d \n" (List.length kqs)) in *)
  let kqs  = group_ks_kqs ks kqs in
  (* let _    = Co.bprintflush mydebug (Printf.sprintf "p_update B: |kqs| = %d \n" (List.length kqs)) in *)
  let kqsm = SM.of_alist kqs in
  let me   = me |> (!Co.cex <?> BS.time "cx_update" (cx_update ks) kqsm) in
  List.fold_left begin fun (b, m) k ->
    SM.finds k kqsm
    |> update m k
    |> Misc.app_fst ((||) b)
  end (false, me.m) ks
  |> Misc.app_snd (fun m -> { me with m = m })


(* API *)
let top s ks =
  ks (* |> List.partition (fun k -> SM.mem k s.m)
     >> (fun (_, badks) -> Co.bprintf mydebug "WARNING: Trueing Unbound KVars = %s \n" (Misc.fsprintf (Misc.pprint_many false "," Sy.print) badks))
     |> fst *)
     |> Misc.flip (p_update s) []
     |> snd

(***************************************************************)
(**************** Query Current Solution ***********************)
(***************************************************************)

let preds_of_bind = function
  | Bot       -> [A.pFalse]
  | NonBot qs -> List.rev_map Q.pred_of_t qs

let raw_read me k = match SM.maybe_find k me.m with
  | None   -> []
  | Some z -> preds_of_bind z

(* API *)
let read me k = (me.assm k) ++ (raw_read me k)

(* API *)
let read_bind s k = failwith "PredAbs.read_bind"


(***************************************************************)
(******************** Qualifier Instantiation ******************)
(***************************************************************)


(* DEBUG ONLY *)
let print_param ppf (x, t) =
  F.fprintf ppf "%a:%a" Sy.print x Sort.print t
let print_params ppf args =
  F.fprintf ppf "%a" (Misc.pprint_many false ", " print_param) args
let print_valid_binding ppf (x,y) =
  F.fprintf ppf "[%a := %a]" Sy.print x Sy.print y
let print_valid_bindings ppf xys =
  F.printf "[%a]" (Misc.pprint_many false "" print_valid_binding) xys

(*
let dupfree_binding xys : bool =
  let ys  = List.map snd xys in
  let ys' = Misc.sort_and_compact ys in
  List.length ys = List.length ys'
*)

let varmatch_ctr = ref 0

let varmatch (x, y) =
  let _     = varmatch_ctr += 1 in
  let (x,y) = Misc.map_pair Sy.to_string (x,y) in
  if x.[0]  = '@' then
    let x'  = Misc.suffix_of_string x 1 in
    Misc.is_prefix x' y
  else true

let sort_compat t1 t2 = Sort.unify [t1] [t2] <> None

let wellformed_qual f q =
  Q.pred_of_t q
  |> A.sortcheck_pred Theories.is_interp f
(* >> (F.printf "\nwellformed: id = %d q = @[%a@] result %b\n" (C.id_of_wf wf) Q.print q) *)
(* NEVER uncomment out the above. *)

(***************************************************************)
(**************** Lazy Instantiation: WF-Index *****************)
(***************************************************************)

let kvars_of_wf wf =
  let check_trivial su = asserts (Su.is_empty su) "non-trivial substitution in WF constraint!" in
  wf |> C.reft_of_wf
     |> C.kvars_of_reft
     >> List.iter (fst <+> check_trivial)
     |> List.map snd

let meet_wf_index k = function
  | (Some (env, v, t), (env', (v', t', _))) when (v=v' && t=t') ->
      env |> SM.filter (fun x _ -> SM.mem x env')
          |> (fun env' -> (env', v, t))
  | (None, (env, (v, t, _))) ->
      (env, v, t)
  | _  ->
      assertf "Conflicting v,t for WF %s" (Sy.to_string k)

let upds_wf_index z wm ks =
  List.fold_left begin fun wm k ->
    SM.add k (meet_wf_index k (SM.maybe_find k wm, z)) wm
  end wm ks

(* API *)
let valid_after_substitution f su y =
  Su.apply su y
  |> (function None -> [y] | Some ye -> E.support ye)
  |> List.for_all f

let kvars_of_bind (x, r) =
  let xv   = (C.vv_of_reft r, A.eVar x) in
  C.kvars_of_reft r |>: (fun (su, k) -> (Su.extend su xv, k))

let kvars_of_c c =
  (C.kvars_of_reft         <| C.rhs_of_t         c) ++
  (Misc.flap kvars_of_bind <| C.kbindings_of_lhs c)

(* MORE DEBUG NOISE -- NEVER DELETE! *)
let pp_ikxts i k xts = F.printf "\n refine_wf_index removes at id = %d, k = %a, xs = %a\n" i  Sy.print k
                 (Misc.pprint_many false ", " Sy.print) (List.map fst xts)

let refine_wf_index wm c =
  let senv  = C.senv_of_t c in
  let ok z  = SM.mem z senv  in
  let ksus  = kvars_of_c c  in (* [(su, k)] *)
  List.fold_left begin fun wm (su, k) ->
    let (xts, v, t) = SM.safeFind k wm "refine_wf_index"                              in
    let (xts', dts) = Misc.tr_partition begin fun (x,t) -> Sort.is_kind t ||
                        valid_after_substitution ok su x
                      end xts
    in SM.add k (xts', v, t) wm
   (* let xts' = Misc.filter (fun (x,t) -> Sort.is_kind t || valid_after_substitution ok su x) xts in
      let _     = pp k xts; pp k xts' in
      let _    = pp_ikxts (C.id_of_t c) k dts in
    *)
  end wm ksus

let create_wf_index_basic ws =
  List.fold_left begin fun wm w ->
    let env = SM.map C.sort_of_reft <| C.env_of_wf w in
    let r   = C.reft_of_wf w                         in
    upds_wf_index (env, r) wm (kvars_of_wf w)
  end SM.empty ws

let create_wf_index_refine_sort cs wm =
  wm |> SM.map (fun (env,v,t) -> ((SM.to_list env), v, t))
     |> Misc.flip (List.fold_left refine_wf_index) cs
     |> SM.map (fun (xts,v,t) -> (SM.of_list xts, v, t))

(* API *)
let create_wf_index cs ws =
  ws |> create_wf_index_basic
     |> ((!Constants.refine_sort) <?> (create_wf_index_refine_sort cs))

(********************************************************************************)
(****** Brute Force (Post-Selection based) Qualifier Instantiation **************)
(********************************************************************************)

type qual_binding = (Sy.t * Sy.t) list

let is_valid_binding (xys : qual_binding) : bool =
  List.for_all varmatch xys

let valid_bindings ys (x,_) =
  ys |> List.map (fun y -> (x, y))
     |> List.filter varmatch

let inst_qual env ys evv (q : Q.t) : Q.t list =
  let vve = (Q.vv_of_t q, evv) in
  match Q.params_of_t q with
  | [] ->
      [(Q.inst q [vve])]
  | xts ->
      xts
      (* >> F.printf "\n\ninst_qual: params q = %a: %a" Q.print q print_params          *)
      |> List.map (valid_bindings ys)                       (* candidate bindings    *)
      |> Misc.product                                       (* generate combinations *)
      (* >> (List.iter (F.printf "\ninst_qual: pre-binds = %a\n" print_valid_bindings)) *)
      |> List.filter is_valid_binding                       (* remove bogus bindings *)
      (* >> (List.iter (F.printf "\ninst_qual: post-binds = %a\n" print_valid_bindings)) *)
      |> List.rev_map (List.map (Misc.app_snd A.eVar))      (* instantiations        *)
      |> List.rev_map (fun xes -> Q.inst q (vve::xes))      (* quals *)
      (* >> (F.printf "\n\ninst_qual: result q = %a:\n%a DONE\n" Q.print q (Misc.pprint_many true "" Q.print)) *)

let inst_binds env =
  env |> SM.to_list
      |> Misc.filter (not <.> Sort.is_func <.> snd)

let inst_ext env vv t qs =
  let _    = Misc.display_tick ()   in
  let ys   = inst_binds env |>: fst in
  let env' = Misc.flip SM.maybe_find (SM.add vv t env) in
  qs |> List.filter (Q.sort_of_t <+> sort_compat t)
     |> Misc.flap   (inst_qual env ys (A.eVar vv))
     |> Misc.filter (wellformed_qual env')

(*****************************************************************************)
(****** Sort Based Qualifier Instantiation ***********************************)
(*****************************************************************************)

(* [ (su', (x,y) : xys) | (su, xys) <- wkl
                        , (y, ty)   <- yts
                        , varmatch (x, y)
                        , Some su'  <- unifyWith su [tx] [ty] ]  *)

let debug_unify_count         = ref 0
let debug_unify_success_count = ref 0

(* BEGIN-ORIGINAL

let ext_bindings yts wkl (x, tx) =
  let yts = List.filter (fun (y,_) -> varmatch (x, y)) yts in
  Misc.tr_rev_flap begin fun (su, xys) ->
    Misc.map_partial begin fun (y, ty) ->
      let u = incr debug_unify_count ; Sort.unifyWith su [tx] [ty] in
      match u with
        | None     -> None
        | Some su' -> let _  = incr debug_unify_success_count in
                      Some (su', (x,y) :: xys)
    end yts
  end wkl

let inst_qual_sorted yts vv t q =
  let (qvv0, t0) :: xts = Q.all_params_of_t q     in
  match BS.time "q-inst-0" (Sort.unify [t0]) [t] with
    | Some su0 ->
        xts |> List.fold_left (ext_bindings yts) [(su0, [(qvv0, vv)])]   (* generate subs-bindings   *)
            |> List.rev_map (List.rev <.> snd)                           (* extract sorted bindings  *)
            |> List.rev_map (List.map (Misc.app_snd A.eVar))             (* instantiations           *)
            |> List.rev_map (Q.inst q)                                   (* quals *)
            (* >> (fun qs -> F.printf "IQS: len qs = %d \n" (List.length qs)) *)
    | None    -> []

let inst_ext_sorted env vv t qs =
  let _   = Misc.display_tick () in
  let yts = inst_binds env       in
  let r   = BS.time "inst-qual-sorted" (Misc.flap (inst_qual_sorted yts vv t)) qs in
  r

let teq t1 t2 =
  let r = (t1 = t2)                                   in
  let _ = F.printf "teq: %s = %s : %b \n"
            (Sort.to_string t1) (Sort.to_string t2) r in
  r
END-ORIGINAL *)

let mono_filter tx tyss = function
  | true  -> List.filter (fun (t, _) -> t = tx) tyss
  | false -> tyss

let mono_unify su tx ty = function
  | true  -> if tx = ty then Some su else None
  | false -> let _ = incr debug_unify_count in
             Sort.unifyWith su [tx] [ty]

let ext_bindings tyss wkl (x, tx) =
  let tyss = tyss
           |>: (fun (t, ys) -> (t, List.filter (fun y -> varmatch (x, y)) ys))
  in
  Misc.tr_rev_flap begin fun (su, xys) ->
    let tx   = Sort.apply su tx     in
    let mono = Sort.is_mono tx      in
    let tyss = mono_filter tx tyss mono in
    Misc.tr_rev_flap begin fun (ty, ys) ->
      let u = mono_unify su tx ty mono in
      match u with
        | None     -> []
        | Some su' -> let _  = incr debug_unify_success_count in
                      List.map (fun y -> (su', (x,y) :: xys)) ys
    end tyss
  end wkl

let inst_qual_sorted tys vv t q =
  let (qvv0, t0) :: xts = Q.all_params_of_t q     in
  match Sort.unify [t0] [t] with
    | Some su0 ->
        xts |> List.fold_left (ext_bindings tys) [(su0, [(qvv0, vv)])]   (* generate subs-bindings   *)
            |> List.rev_map (List.rev <.> snd)                           (* extract sorted bindings  *)
            |> List.rev_map (List.map (Misc.app_snd A.eVar))             (* instantiations           *)
            |> List.rev_map (Q.inst q)                                   (* quals *)
            (* >> (fun qs -> F.printf "IQS: len qs = %d \n" (List.length qs)) *)
    | None    -> []

let inst_ext_sorted env vv t qs =
  let _   = Misc.display_tick () in
  let tys = inst_binds env
            |> Misc.kgroupby snd
            |> List.map (fun (ty, yts) -> (ty, List.map fst yts))  in
  let r   = BS.time "inst-qual-sorted" (Misc.flap (inst_qual_sorted tys vv t)) qs in
  r

(***************************************************************)
(**************** Lazy Instantiation ***************************)
(***************************************************************)

let inst_ext qs ckEnv env v t  : Q.t list =
  let instf = if !Co.sorted_quals then inst_ext_sorted else inst_ext in
  let env' = Misc.flip SM.maybe_find (SM.add v t ckEnv) in
  qs |> instf env v t
     |> Misc.filter (wellformed_qual env')

let is_non_trivial_var me c su =
  let senv = C.senv_of_t c in
  let ok z = SM.mem z senv in
  fun y _ -> valid_after_substitution ok su y

(* RJ: DO NOT DELETE EVER! *)
let ppBinding k zs =
  F.printf "ppBind %a := %a \n"
    Sy.print k
    (Misc.pprint_many false ", " Q.print) zs

(* API *)
let lazy_instantiate_with me c k su : Q.t list =
  let (env,v,t) = SM.safeFind k me.wm "lazy_instantiate"       in
  let env'      = SM.filter (is_non_trivial_var me c su) env in
  inst_ext me.qs env env' v t
  (* >> ppBinding k *)
  |> ((++) (SM.find_default [] k me.om))


(***************************************************************)
(**************** Refinement ***********************************)
(***************************************************************)

(*** {{{ KEEP AROUND FOR DEBUG PRINTING SIGH.
let rhs_cands me = function
  | C.Kvar (su, k) ->
      k
  (* >> (fun k -> Co.bprintflush mydebug ("rhs_cands: k = "^(Sy.to_string k)^"\n")) *)
      |> p_read me
  (* >> (fun xs -> Co.bprintflush mydebug ("rhs_cands: size="^(string_of_int (List.length xs))^" BEGIN \n")) *)
      |>: (Misc.app_snd (Misc.flip A.substs_pred su))
  (* >> (fun xs -> Co.bprintflush mydebug ("rhs_cands: size="^(string_of_int (List.length xs))^" DONE\n")) *)
  | _ -> []

let get_lhs me c   = BS.time "preds_of_lhs" (C.preds_of_lhs (read me)) c
}}} *)


let bind_read me k = SM.find_default Bot k me.m

let is_bot_reft me (_,_,ras) =
  List.exists begin function C.Conc _ -> false | C.Kvar (_,k) ->
    (bind_read me k = Bot)
  end ras

let is_bot_lhs me c =
  C.kbindings_of_lhs c
  |>: snd
  |> List.exists (is_bot_reft me)

let is_bot_rhs me c =
  is_bot_reft me <| C.rhs_of_t c

let make_cand k su q =
  let qp  = Q.pred_of_t q       in
  let qp' = A.substs_pred qp su in
  ((k, q), qp')

let quals_of_bind me c k su = function
  | NonBot qs ->
      (qs, me)
  | Bot       ->
      let qs      = lazy_instantiate_with me c k su           in
      let (_, me) = p_update me [k] (qs |>: (fun q -> (k,q))) in
      (qs, me)

(* only called when ALL RHS k are NONBOT *)
let rhs_cands_noinst me = function
  | C.Kvar (su, k) -> begin match bind_read me k with
                        | Bot       -> assertf "rhs_cands_noinst"
                        | NonBot qs -> qs |>: make_cand k su
                      end
  | _              -> []

let rhs_cands_noinst me c =
  c |> C.rhs_of_t
    |> thd3
    |> BS.time "rhs_cands" (Misc.flap (rhs_cands_noinst me))

(* only called when SOME RHS k is BOT *)
let rhs_cands_inst c me = function
  | C.Conc _ ->
      (me, [])
  | C.Kvar (su, k) ->
      let (qs, me) = SM.safeFind k me.m "rhs_cands" |> quals_of_bind me c k su  in
      (me, qs |>: make_cand k su)

let rhs_cands_inst me c =
  let (_, _, ras) = C.rhs_of_t c                               in
  let (me, zs)    = Misc.mapfold (rhs_cands_inst c) me ras in
  (Misc.flatten zs, me)

let lhs_preds me c =
  let lps = BS.time "lhs_preds" (C.preds_of_lhs (read me)) c in
  (lps, me)

let refine_sort_bot_rhs me c =
  let (lps, me) = lhs_preds me c                                          in
  let (rcs, me) = BS.time "rsb 2" (rhs_cands_inst me) c                   in
  let senv      = C.senv_of_t c                                           in
  let rcs       = Misc.filter (fun (_,p) -> C.wellformed_pred senv p) rcs in
  (true, me, lps, rcs)

let refine_sort_first_time me c =
  let (lps, me) = lhs_preds me c                                          in
  let rcs       = rhs_cands_noinst me c                                   in
  let senv      = C.senv_of_t c                                           in
  let rcs'      = Misc.filter (fun (_,p) -> C.wellformed_pred senv p) rcs in
  (List.length rcs != List.length rcs', me, lps, rcs')

let refine_sort_default me c =
  let (lps, me) = lhs_preds me c        in
  let rcs       = rhs_cands_noinst me c in
    (false, me, lps, rcs)

let refine_sort me c =
  if is_bot_rhs me c then
    BS.time "refine-sort-bot" (refine_sort_bot_rhs me) c
  else if not (IS.mem (C.id_of_t c) (me.seen)) then
    BS.time "refine-sort-first" (refine_sort_first_time me) c
  else
    BS.time "refine-sort-default" (refine_sort_default me) c

let is_trivial_rhs me c =
  let is_trivial_refa me = function
    | C.Conc _     -> true
    | C.Kvar (_,k) -> bind_read me k = NonBot []
  in
    C.rhs_of_t c
    |> C.ras_of_reft
    |> List.for_all (is_trivial_refa me)

let is_trivial_c me c = is_bot_lhs me c || is_trivial_rhs me c

let refine_match me lps rcs =
  let lt      = PH.create 17                                  in
  let _       = List.iter (fun p -> PH.add lt p ()) lps       in
  let (x1,x2) = List.partition (fun (_,p) -> PH.mem lt p) rcs in
  let _       = me.stat_matches += (List.length x1)           in
  (List.map fst x1, x2)

let check_tp me env vv t lps =  function [] -> [] | rcs ->
  me.tpc#set_filter env vv lps rcs
  >> (fun _  -> me.stat_tp_refines    += 1)
  >> (fun _  -> me.stat_imp_queries   += List.length rcs)
  >> (fun rv -> me.stat_valid_queries += List.length rv)

let refine_tp me c lps x2 =
  if C.is_simple c then
    (me.stat_simple_refines += 1) >| []
  else
    let senv = C.senv_of_t c in
    let vv   = C.vv_of_t c   in
    let t    = C.sort_of_t c in
    BS.time "check tp" (check_tp me senv vv t lps) x2

let refine_update me c kqs1 kqs2 =
  let ks = C.rhs_of_t c |> C.kvars_of_reft |>: snd in
  p_update me ks (kqs1 ++ kqs2)

let refine me c =
  if BS.time "is_triv" (is_trivial_c me) c then
    (false, me)
  else
    let (ch, me, lps, rcs) = BS.time "refine-sort" (refine_sort me) c     in
    if BS.time "refine-lhs-contra" (List.exists P.is_contra) lps then
      let _          = me.stat_unsatLHS += 1                              in
      let _          = me.stat_umatches += List.length rcs                in
      (ch, me)
    else
      let rcs        = BS.time "refine-cands"  (List.filter (fun (_,p) -> not (P.is_contra p))) rcs in
      let (kqs1, x2) = BS.time "refine-match"  (refine_match me lps) rcs                            in
      let kqs2       = BS.time "refine-tp"     (refine_tp me c lps) x2                              in
      let (ch', me)  = BS.time "refine-update" (refine_update me c kqs1) kqs2                       in
      (ch || ch', me)

let refine me c =
  let (ch, me) = BS.time "PA.refine" (refine me) c in
  (ch, {me with seen = IS.add (C.id_of_t c) me.seen})

let refine me c =
  let me      = me |> (!Co.cex <?> cx_iter c)     in
  let (b, me) = refine me c                       in
  let me      = me |> (!Co.cex <?> cx_ctrace b c) in
  (b, me)

(***************************************************************)
(************************* Satisfaction ************************)
(***************************************************************)

let unsat me c =
  let s        = read me      in
  let (vv,t,_) = C.lhs_of_t c in
  let lps      = C.preds_of_lhs s c  in
  let rhsp     = c |> C.rhs_of_t |> C.preds_of_reft s |> A.pAnd in
  let k        = Sy.of_string "k" in
  let kq       = (k, Q.create k k Sort.t_int [] A.pTrue) in
  not ((check_tp me (C.senv_of_t c) vv t lps [(kq, rhsp)]) = [kq])

(****************************************************************)
(************* Minimization: For Prettier Output ****************)
(****************************************************************)

(*
let canonize_subs =
  Su.to_list <+> List.sort (fun (x,_) (y,_) -> compare x y)

let subst_leq =
  Misc.map_pair canonize_subs <+> Misc.isPrefix
*)

let args_leq q1 q2 =
  let qArgs = List.map snd <.> Q.args_of_t in
  try List.for_all2 (=) (qArgs q1) (qArgs q2) with _ -> false

(* P(v,x,y,z) => Q(v,x) if P => Q held and _intersection_ of args match. *)
let def_leq s q1 q2 =
     Q2S.mem (Q.name_of_t q1, Q.name_of_t q2) s.qleqs && args_leq q1 q2

let pred_of_bind_name q =
  let name = q |> Q.name_of_t                 in
  let args = q |> Q.args_of_t |> List.map snd in
  A.pBexp (A.eApp (name, args))

let pred_of_bind_raw = Q.pred_of_t

let pred_of_bind q =
  if !Co.shortannots
  then pred_of_bind_name q
  else pred_of_bind_raw q

let min_binds_bot ds =
  match Misc.list_find_maybe (P.is_contra <.> pred_of_bind_raw) ds with
    | None   -> ds
    | Some d -> [d]

(* API *)
let min_binds s ds = ds |> min_binds_bot |> Misc.rootsBy (def_leq s)
let min_read s k   = SM.find_default Bot k s.m |> function
                      | Bot       -> [A.pFalse]
                      | NonBot qs -> qs |> min_binds s |>: pred_of_bind
let min_read s k   = if !Co.minquals then min_read s k else read s k
let min_read s k   = BS.time "min_read" (min_read s) k

let close_env qs sm =
  qs |> Misc.flap   (Q.pred_of_t <+> P.support)
     |> Misc.filter (not <.> Misc.flip SM.mem sm)
     |> Misc.map    (fun x -> (x, Sort.t_int))
     |> SM.of_list
     |> SM.extend sm

let rename_vv q q' =
  List.combine (Q.all_params_of_t q |>: fst) (Q.all_params_of_t q' |>: fst)
  |> List.filter (fun (x, y) -> not (x = y))
  |> List.map (fun (x, y) -> (y, A.eVar x))
  |> Su.of_list
  |> A.substs_pred (Q.pred_of_t q')
  |> (fun p' -> (q', p'))

let sm_of_qual sm q =
  q |> Q.all_params_of_t
    |> SM.of_list
    |> SM.extend sm

(*  check_leq tp sm q qs = [q' | q' <- qs, Z3 |- q => q'] *)
let check_leq (tp : ProverArch.prover) sm (q : Q.t) (qs : Q.t list) : Q.t list =
  let vv  = Q.vv_of_t q in
  let lps = [Q.pred_of_t q] in
  let sm  = q |> sm_of_qual sm |> close_env qs in
  qs |> List.map (rename_vv q) (* (fun q -> (q, Q.pred_of_t q)) *)
     (* >> (List.map fst <+> F.printf "CHECK_TP: %a IN %a \n" Q.print q pprint_qs) *)
     |> tp#set_filter sm vv lps
     (* >> F.printf "CHECK_TP: %a OUT %a \n" Q.print q pprint_qs *)

let qimps_of_partition tp sm qs =
  foreach qs begin fun q ->
    let qs' = check_leq tp sm q qs in
    foreach qs' begin fun q' ->
      (q, q')
    end
  end

let wellformed_qual sm q =
  let sm = sm_of_qual sm q in
  A.sortcheck_pred Theories.is_interp (fun x -> SM.maybe_find x sm) (Q.pred_of_t q)
(*   >> (fun res ->F.printf "wellformed_qual: q = %a, res = %b\n" Q.print q res)
 *)

let qleqs_of_qs ts sm cs ps qs  =
  let tp = TpNull.create ts sm cs ps in
  qs |> Misc.filter (wellformed_qual sm)
     |> Misc.groupby (List.map snd <.> Q.all_params_of_t) (* Q.sort_of_t *)
     |> Misc.flap (qimps_of_partition tp sm)
     |> Misc.flatten
     |> Misc.map (Misc.map_pair Q.name_of_t)
     |> Q2S.of_list


(*************************************************************************)
(*************************** Creation ************************************)
(*************************************************************************)

let create_qleqs ts sm ps consts qs =
  if !Co.minquals
  then BS.time "Annots: make qleqs" (qleqs_of_qs ts sm consts ps) qs
  else Q2S.empty

let create obm cs ws ts sm ps consts assm qs bm =
  { m     = bm
  ; om    = SM.map (function Bot -> [] | NonBot qs -> qs) obm
  ; wm    = create_wf_index cs ws
  ; assm  = assm
(*; qm    = qs |>: Misc.pad_fst Q.name_of_t |> SM.of_list *)
  ; qs    = qs
  ; qleqs = Misc.with_ref_at Constants.strictsortcheck false
              (fun () -> create_qleqs ts sm consts ps qs)
  ; tpc   = TpNull.create ts sm ps consts
  ; seen  = IS.empty

  (* Counterexamples *)
  ; step     = 0
  ; ctrace   = IM.empty
  ; lifespan = SM.empty

  (* Stats *)
  ; stat_simple_refines = ref 0
  ; stat_tp_refines     = ref 0; stat_imp_queries    = ref 0
  ; stat_valid_queries  = ref 0; stat_matches        = ref 0
  ; stat_umatches       = ref 0; stat_unsatLHS       = ref 0
  ; stat_emptyRHS       = ref 0
  }

(***************************************************************)
(****************** Sort Check Based Refinement ****************)
(***************************************************************)
(* LAZYINST
let refts_of_c c =
  [ C.lhs_of_t c ; C.rhs_of_t c] ++ (C.env_of_t c |> C.bindings_of_env |>: snd)

let refine_sort_reft env me ((vv, so, ras) as r) =
  let env' = SM.add vv r env in
  let ks   = r |> C.kvars_of_reft |>: snd in
  (* let _    = let s =  String.concat ", " (List.map Sy.to_string ks) in Co.bprintflush mydebug ("\n refine_sort_reft ks = "^s^"\n")  in  *)
  ras
  |> Misc.flap (rhs_cands me) (* OMFG blowup due to FLAP if kv appears multiple times...*)
  |> Misc.filter (fun (_, p) -> C.wellformed_pred env' p)
  |> List.rev_map fst
(* |> (fun xs -> Co.bprintflush mydebug (Printf.sprintf "refine_sort_reft map: size = %d\n" (List.length xs));
                List.rev_map fst xs)
  >> (fun _ -> Co.bprintflush mydebug "\n refine_sort_reft TICK 4 \n")
  *)
  |> p_update me ks
  |> snd

let refine_sort me c =
  let env = C.env_of_t c in
  c (* >> (fun _ -> Co.bprintflush mydebug ("\n refine_sort TICK 0 id = "^(string_of_int (C.id_of_t c))^"\n")) *)
    |> refts_of_c
    |> List.fold_left (refine_sort_reft env) me
    (* >> (fun _ -> Co.bprintflush mydebug "\n refine_sort TICK 2 \n") *)
*)

(****************************************************************************)
(****************** APPLYING FACTS FOR INCREMENTAL SOLVING ******************)
(****************************************************************************)

(* LAZYINST

(* Take in a solution of things that are known to be true, kf. Using
   this, we can prune qualifiers whose negations are implied by
   information in kf *)
let update_pruned ks me fqm =
  List.fold_left begin fun m k ->
    if not (SM.mem k fqm) then m else
      let false_qs = SM.safeFind k fqm "update_pruned 1" in
      let qs = SM.safeFind k m "update_pruned 2"
               |> List.filter (fun q -> (not (List.mem (k, q) false_qs)))
      in SM.add k qs m
  end me.m ks

let apply_facts_c kf me c =
  let env = C.senv_of_t c in
  let (vv, t, lras) = C.lhs_of_t c in
  let (_,_,ras) as rhs = C.rhs_of_t c in
  let ks = rhs |> C.kvars_of_reft |> List.map snd in
  let lps = C.preds_of_lhs kf c in (* Use the known facts here *)
  let rcs = Misc.flap (rhs_cands me) ras in
    if rcs = [] then               (* Nothing on the right hand side *)
      me
    else if check_tp me env vv t lps [(0, A.pFalse)] = [0] then
      me
    else
      let rcs = List.filter (fun (_,p) -> not (P.is_contra p)) rcs
                |> List.map (fun (x,p) -> (x, A.pNot p)) in
	(* can we prove anything on lhs implies something on rhs is false? *)
      let fqs = BS.time "apply_facts tp" (check_tp me env vv t lps) rcs in
      let fqm = fqs |> Misc.kgroupby fst |> SM.of_list in
	  {me with m = BS.time "update pruned" (update_pruned ks me) fqm}

let apply_facts cs kf me =
  let numqs = me.m |> Symbol.SMap.to_list
              |> List.map snd |> List.concat |> List.length in
  let sol   = List.fold_left (apply_facts_c kf) me cs in
  let numqs' = sol.m |> Symbol.SMap.to_list
               |> List.map snd |> List.concat |> List.length in
  let _ = Printf.printf "Started with %d, proved %d false\n" numqs (numqs-numqs') in
    sol

*)

(* LAZYINST: map each KVAR to BOT *)
let initial_solution c =
  c.Cg.ws
  |>  Misc.flap kvars_of_wf
  |>: (fun k -> (k, Bot))
  |>  SM.of_list

(* API *)
let create c = function
  | None ->
      initial_solution c
      |> create c.Cg.bm c.Cg.cs c.Cg.ws c.Cg.ts c.Cg.uops c.Cg.ps c.Cg.cons c.Cg.assm c.Cg.qs
      (* LAZYINST: this is factored into the canonical env for each K
      >> (fun _ -> Co.bprintflush mydebug "\nBEGIN: refine_sort\n")
      |> ((!Constants.refine_sort) <?> Misc.flip (List.fold_left refine_sort) c.Cg.cs)
      >> (fun _ -> Co.bprintflush mydebug "\nEND: refine_sort\n")
      *)
  | _ -> assertf "PredAbs.create: does not support facts"

(* API *)
let empty () = create Cg.empty None

(* API *)
let meet me you = {me with m = SM.extendWith (fun _ -> meet_bind) me.m you.m}

(****************************************************************)
(************* Simplify Solution Using min_read *****************)
(****************************************************************)

(* let minb s bs = min_binds s bs
              >> Printf.printf "minBinds: [%a] \n\n"  pprint_ds
 *)

let simplify s = { s with m = SM.map begin function
                                | Bot       -> Bot
                                | NonBot qs -> NonBot (min_binds s qs)
                              end s.m
                 }

(************************************************************************)
(****************** Counterexample Generation ***************************)
(************************************************************************)


let ctr_examples me cs ucs =
  let cx = CX.create me.tpc (read me) cs me.ctrace me.lifespan in
  List.map (CX.explain cx) ucs


(*******************************************************************************)
(******************************** Profile/Stats ********************************)
(*******************************************************************************)

let print_m ppf s =
  SM.iter begin fun k -> function
    | Bot       -> F.fprintf ppf "solution: %a := [%a] \n\n"  Sy.print k pprint_bind Bot
    | NonBot ds -> ds
                   |> (<?>) (!Co.minquals) (min_binds s)
                   |> F.fprintf ppf "solution: %a := [%a] \n\n"  Sy.print k pprint_ds
  end s.m

let print_qs ppf s =
  s.qs >> (fun _ -> F.fprintf ppf "//QUALIFIERS \n\n")
       |> F.fprintf ppf "%a" (Misc.pprint_many true "\n" Q.print)
(*  |> List.iter (F.fprintf ppf "%a" Q.print) *)
       |> ignore

(* API *)
let print ppf s = s >> print_m ppf >> print_qs ppf |> ignore


let botInt = function
  | Bot       -> 1
  | NonBot qs -> if List.exists (Q.pred_of_t <+> P.is_contra) qs then 1 else 0

let bindSize = function
  | Bot       -> 0
  | NonBot x  -> List.length x

(* API *)
let print_stats ppf me =
  let (sum, max, min, bot) =
    (SM.fold (fun _ b x -> (+) x (bindSize b)) me.m 0,
     SM.fold (fun _ b x -> max x (bindSize b)) me.m min_int,
     SM.fold (fun _ b x -> min x (bindSize b)) me.m max_int,
     SM.fold (fun _ b x -> x      + botInt b)  me.m 0) in
  let n   = SM.length me.m in
  let avg = (float_of_int sum) /. (float_of_int n) in
  F.fprintf ppf "# Vars: (Total=%d, False=%d) Quals: (Total=%d, Avg=%f, Max=%d, Min=%d)\n"
    n bot sum avg max min;
  F.fprintf ppf "#Iteration Profile = (si=%d tp=%d unsatLHS=%d emptyRHS=%d) \n"
    !(me.stat_simple_refines) !(me.stat_tp_refines)
    !(me.stat_unsatLHS) !(me.stat_emptyRHS);
  F.fprintf ppf "#Queries: umatch=%d, match=%d, ask=%d, valid=%d\n"
    !(me.stat_umatches) !(me.stat_matches) !(me.stat_imp_queries)
    !(me.stat_valid_queries);
  F.fprintf ppf "#UnifyWith: (%d/%d)\n"
    !debug_unify_success_count
    !debug_unify_count;
  me.tpc#print_stats ppf

(* API *)
let save fname s =
  let oc  = open_out fname in
  let ppf = F.formatter_of_out_channel oc in
  F.fprintf ppf "@[%a@] \n" print s;
  close_out oc

let key_of_quals qs =
  qs |> List.map P.to_string
     |> List.sort compare
     |> String.concat ","

(* API *)
let mkbind qs = assertf "PredAbs.mkBind not supported in lazyinst" (* NonBot qs *)(* Misc.flatten <+> Misc.sort_and_compact *)

(* API *)
let dump s = ()
(*
  s.m
  |> SM.to_list
  |> List.map (snd <+> preds_of_bind)
  |> Misc.groupby key_of_quals
  |> List.map begin function
     | []             -> assertf "impossible"
     | (ps::_ as pss) -> Co.bprintf mydebug "SolnCluster: preds %d = size %d \n" (List.length ps) (List.length pss)
     end
  |> ignore
*)
