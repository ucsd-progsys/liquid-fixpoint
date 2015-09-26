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
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

(**
 * This module implements a Hash-Consed representation for sorts to
 * enable fast unification.
 * Each sub-sort is paired with a unique int ID, which enables constant
 * time hashing; However, one must take care when using DAGS:
 * (1) they can only be constructed using the appropriate functions
 * (2) when destructed via pattern-matching, one must discard the ID
 *)



module F    = Format
module Misc = FixMisc
open Misc.Ops
module SM = Misc.StringMap
module IS = Misc.IntSet
type tag  = int

let mydebug = false

type loc =
  | Loc  of string
  | Lvar of int
  | LFun

type tycon = string

(* type t = t_int * tag *)

type t = t_int

and t_int =
  | Int
  | Real
  | Bool
  | Obj
  | Var of int              (* type-var *)
  | Ptr of loc              (* c-pointer *)
  | Func of int * t list    (* type-var-arity, in-types @ [out-type]         *)
  | Num                     (* kind, for numeric tyvars -- ptr(loc(s))    -- *)
  | Frac                    (* kind, for fractional tyvars -- ptr(loc(s)) -- *)
  | App of tycon * t list   (* type constructors *)

type sub = { locs: (int * string) list
           ; vars: (int * t) list;
           }

let tycon_string x = x
    (*
    let is_loc_string s =
      let re = Str.regexp "[a-zA-Z]+[0-9]+" in
      Str.string_match re s 0

    let loc_of_string = fun s -> let _ = asserts (is_loc_string s) in Loc s
    let loc_of_index  = fun i -> Lvar i
    *)

let t_num       = Num
let t_frac      = Frac
let t_obj       = Obj
let t_bool      = Bool
let t_int       = Int
let t_real      = Real
let t_generic   = fun i -> let _ = asserts (0 <= i) "t_generic: %d" i in Var i
let t_ptr       = fun l -> Ptr l
let t_func      = fun i ts -> Func (i, ts)
let tycon s     = s
let tc_app      = "FAppTy"

    (* let tycon_re    = Str.regexp "[A-Z][0-9 a-z A-Z '.']"
     * function | s when Str.string_match tycon_re s 0 -> s
                | s -> assertf "Error: Invalid tycon: %s" s
     *)

let t_app = List.fold_left (fun t1 t2 -> App (tc_app, [t1; t2]))

let t_app_tc c ts = if c = tc_app then
                      (App (c, ts))
                    else
                      t_app (App (c, [])) ts

let loc_to_string = function
  | Loc s  -> s
  | Lvar i -> string_of_int i
  | LFun   -> "<fun>"

let rec to_string = function
  | Var i        -> Printf.sprintf "@(%d)" i
  | Int          -> "int"
  | Real         -> "real"
  | Bool         -> "bool"
  | Obj          -> "obj"
  | Num          -> "num"
  | Frac         -> "frac"
  | Ptr l        -> loc_to_string l
  (* Printf.sprintf "ptr(%s)" (loc_to_string l) *)
  | Func (n, ts) -> ts |> List.map to_string
                       |> String.concat " ; "
                       |> Printf.sprintf "func(%d, [%s])" n
  | App (c, ts)  -> ts |> List.map to_string_arg
                       |> String.concat " "
                       |> Printf.sprintf "%s %s" c

and to_string_arg t = match t with
  | App (_, _) -> Printf.sprintf "(%s)" (to_string t)
  | _          -> to_string t

let to_string_short = function
  | Func _ -> "func"
   (* | Ptr _  -> "ptr" *)
  | t      -> to_string t

let print fmt t =
  t |> to_string
    |> Format.fprintf fmt "%s"

let sub_to_string {locs = ls; vars = vs} =
  let lts = fun (i, s) -> Printf.sprintf "(%d := %s)" i s in
  let vts = fun (i, t) -> Printf.sprintf "(%d := %s)" i (to_string t) in
  Printf.sprintf "locs := %s, vars := %s \n"
    (String.concat "" (List.map lts ls))
    (String.concat "" (List.map vts vs))

let rec map f = function
  | Func (n, ts) -> Func (n, List.map (map f) ts)
  | App  (c, ts) -> App  (c, List.map (map f) ts)
  | t            -> f t

let rec fold f b = function
  | Func (n, ts) as t -> List.fold_left (fold f) (f b t) ts
  | t                 -> f b t

let subs_tvar ts =
  map begin function
    | Var i -> Misc.do_catchf "ERROR: subs_tvar" (List.nth ts) i
    | t     -> t
  end

let is_bool = function
  | Bool -> true
  | _    -> false

let is_int = function
  | Int -> true
  | _   -> false

let is_real = function
  | Real -> true
  | _    -> false

let is_func = function
      | Func _ -> true
      | _   -> false

let is_kind = function
      | Num    -> true
      | _      -> false


    (* (L t1 t2 t3) is now encoded as
        ---> (((L @ t1) @ t2) @ t3)
        ---> App(@, [App(@, [App(@, [L[]; t1]); t2]); t3])
        The following decodes the above as
     *)
let rec app_args_of_t acc = function
  | App (c, [t1; t2]) when c = tc_app -> app_args_of_t (t2 :: acc) t1
  | App (c, [])                       -> (c, acc)
  | t                                 -> (tc_app, t :: acc)

      (*
      | Ptr (Loc s)                       -> (tycon s, acc)
      | t                                 -> assertf "app_args_of_t: unexpected t1 = %s" (to_string t)
      *)

let app_of_t = function
  | App (c, _) as t when c = tc_app   -> Some (app_args_of_t [] t)
  | App (c, ts)                       -> Some (c, ts)
  | _                                 -> None

let func_of_t = function
  | Func (i, ts) -> let (xts, t) = ts |> Misc.list_snoc |> Misc.swap in
                        Some (i, xts, t)
  | _            -> None

let ptr_of_t = function
  | Ptr l -> Some l
  | _     -> None

    (* Sleazy Hack for C pointers. Make this go away... *)

let compat t1 t2 = match t1, t2 with
  | Int, (Ptr _) -> true
  | (Ptr _), Int -> true
  | _            -> t1 = t2

    (* {{{
    let concretize ts = function
      | Func (n, ats) when n = List.length ts ->
          Func (n, List.map (subs_tvar ts) ats)
      | _ ->
          assertf "ERROR: bad application"

    let is_monotype t =
      fold (fun b t -> b && (match t with Var _ -> false | _ -> true)) true t
    }}} *)


let lookup_var = fun s i -> try Some (List.assoc i s.vars) with Not_found -> None
let lookup_loc = fun s j -> try Some (List.assoc j s.locs) with Not_found -> None

let index = ref 0

let makeFresh n =
  let rec go i =
    if i < n then (let x = !index in incr index; (i,x)::go (i+1)) else [] in
  go 0

let rec refresh su = function
  | Int -> Int
  | Real -> Real
  | Bool -> Bool
  | Obj  -> Obj
  | Var i -> (try (Var (snd (List.find (fun (j, _) -> j == i) su)))
              with Not_found -> Var i)
  | Ptr l -> Ptr l
  | Func(n, ts) ->  let su' = List.filter (fun (i,_) -> i>= n) su in  Func(n, List.map (refresh su') ts)
  | Num  -> Num
  | Frac -> Frac
  | App(tc, ts) -> App(tc, List.map (refresh su) ts)

let refresh_tfun (tyArity, i_ts', o_t') =
      let freshMap = makeFresh tyArity in
      let i_ts = List.map (refresh freshMap) i_ts' in
      let o_t  = refresh freshMap o_t' in (tyArity, i_ts, o_t)


let rec unifyt s = function
      | Num,_ | _, Num -> None
      | ct, (Var i)
      | (Var i), ct
        (* when ct != Bool *) ->
          begin match lookup_var s i with
          | Some ct' when ct = ct' -> (*let _ = F.printf "\nUnify YES! %s \t - \t  %s" (to_string ct) (to_string (Var i)) in *) Some s
          | Some ct''              -> (*let _ = F.printf "\nUnify No! %s \t /= %s \t - \t  %s"  (to_string ct) (to_string ct'') (to_string (Var i)) in *) None
          | None                   -> (*let _ = F.printf "\nUnify Add! %s \t - \t  %s" (to_string ct) (to_string (Var i)) in *) Some {s with vars = (i,ct) :: s.vars}
          end

      | Ptr LFun, Ptr _
      | Ptr _, Ptr LFun -> Some s
      | Ptr (Loc cl), Ptr (Lvar j)
      | Ptr (Lvar j), Ptr (Loc cl) ->
          begin match lookup_loc s j with
          | Some cl' when cl' = cl -> Some s
          | Some _                 -> None
          | None                   -> Some {s with locs = (j,cl) :: s.locs}
          end

      | App (c1, t1s), App (c2, t2s)
        when c1 = c2 && List.length t1s = List.length t2s ->
          Misc.maybe_fold unifyt s (List.combine t1s t2s)

      | (t1, t2) when t1 = t2 ->
         Some s
      (* Adding code for polymorphic arguments *)
      | Func (i,[t1]), t2 ->
        begin
          let freshMap = makeFresh i in
          let t1'      = refresh freshMap t1 in
          unifyt s (t1', t2)
        end
      | t1, Func (i,[t2]) ->
        begin
          let freshMap = makeFresh i in
          let t2'      = refresh freshMap t2 in
          unifyt s (t1, t2')
        end
      | _        -> None

let empty_sub = {vars = []; locs = []}

let unifyWith s ats cts =
      let _ = asserts (List.length ats = List.length cts) "ERROR: unify sorts" in
      List.combine ats cts
      |> Misc.maybe_fold unifyt s

(*      >> (fun so -> Printf.printf "unify: [%s] ~ [%s] = %s \n"
                      (String.concat "; " (List.map to_string ats))
                      (String.concat "; " (List.map to_string cts))
                      (match so with None -> "NONE" | Some s -> sub_to_string s))
*)

let unify = unifyWith empty_sub

let apply s =
      map begin fun t -> match t with
          | Var i        -> (match lookup_var s i with Some t' -> t' | _ -> t)
          | Ptr (Lvar j) -> (match lookup_loc s j with Some l -> Ptr (Loc l) | _ -> t)
          | _            -> t
      end

let rec fold f acc t = match t with
      | Var _ | Int | Real | Bool | Obj | Num | Ptr _
        -> f acc t
      | Func (_, ts) | App (_, ts)
        -> List.fold_left (fold f) (f acc t) ts

let vars_of_t = fold begin fun acc -> function
      | Var i -> i :: acc
      | _     -> acc
    end []

let locs_of_t = fold begin fun acc -> function
      | Ptr (Loc l) -> l :: acc
      | _           -> acc
    end []

let subst_locs_vars lim = map begin function
      | Ptr (Loc l) when SM.mem l lim -> Var (SM.find l lim)
      | t                             -> t
    end

    (* API *)
let generalize ts =
      let locs = ts |> Misc.flap locs_of_t |> Misc.sort_and_compact       in
      let idx  = ts |> Misc.flap vars_of_t |> Misc.list_max (-1) |> (+) 1 in
      let lim  = Misc.index_from idx locs |>: Misc.swap |> SM.of_list     in
      List.map (subst_locs_vars lim) ts

    (* API *)
let sub_args s = List.sort compare s.vars

    (* API *)
let check_arity n s =
      let n_vars = s.vars |>: fst |> Misc.sort_and_compact |> List.length  in
      n == n_vars





(***************************************************************************)
(*********** New Type Checking Expressions and Predicates ******************)
(***************************************************************************)

(* let logf s = print_string ("\nLOG: " ^ s) *)
let logf s = ()

exception UnificationError of string

(* Substitutions *)


(* TODO: turn them into a map *)
let sub_empty = []

let rec sub_find_with_default t i = function
  | [] -> t
  | ((j,tj)::s) when i = j -> tj
  | (_::s) -> sub_find_with_default t i s

let sub_singleton i t = [(i, t)]

let vindex = ref 0
let init_ti _ = vindex := 42

let rec sub_fresh i =
  if i == 0
    then []
    else let j = !vindex in
         incr vindex;
         (i-1, Var j) :: sub_fresh (i-1)

let fresh_var _ =
  let j = !vindex in
  incr vindex;
  Var j

(* application of subtitutions *)

let rec apply_ty s = function
  | Var i        -> sub_find_with_default (Var i) i s
  | Func (n, ts) -> Func (n,  List.map (apply_ty s) ts)
  | App (tc, ts) -> App  (tc, List.map (apply_ty s) ts)
  | t            -> t



let rec free_vars = function
  | Var i -> IS.singleton i
  | Func(n, ts) -> List.map (fun t ->
                                   free_vars t |> IS.filter (fun i -> i>=n)
                                 ) ts
                        |> List.fold_left IS.union IS.empty
  | App (_, ts) -> List.map free_vars ts
                        |> List.fold_left IS.union IS.empty
  | _ -> IS.empty


let rec sub_free t = IS.elements (free_vars t)
                   |> List.map (fun i -> (i, fresh_var ()))

let instantiate_ty t = match t with
  | Func (n ,ts) -> let s = sub_fresh n in
                         let r = Func(0, List.map (apply_ty s) ts) in
                         let _ = logf ("instantiate_ty: " ^ (to_string t) ^ " is " ^ (to_string r)) in
                         (s, r)
  | _                 -> (sub_empty, t)



let splitArgs = function
  | Func (_, ts) -> List.rev ts |> fun xs -> (List.tl xs |> List.rev, List.hd xs)
  | t -> ([], t)

let sub_apply s1 s2 = List.map (fun (i, t) -> (i, apply_ty s1 t)) s2



let sub_compose s1 s2 = sub_apply s2 s1 ++ s2


let is_free i t = IS.exists (fun j -> j == i) (free_vars t)

let var_asgn i t =
  if (Var i) == t
    then sub_empty
    else if is_free i t
    then UnificationError ("var_asgn " ^ string_of_int i ^ " is free in  " ^ to_string t) |> raise
    else sub_singleton i t

let rec mgu i t1 t2 =
 let _ = logf ("  of " ^ string_of_int i ^ " " ^ to_string t1 ^ " and " ^ to_string t2) in
 match (t1, t2) with
  | Int, Int

  (* Some pointer casting *)
  | Int, Ptr _
  | Ptr _, Int

  | Real, Real
  | Bool, Bool
  | Obj, Obj
  | Num, Num
  | Frac, Frac -> sub_empty
  | Ptr l1, Ptr l2 when l1 = l2  -> sub_empty
  | Var i, Var j when i = j -> sub_empty
  | Var i, t
  | t, Var i -> var_asgn i t
  | Func (n1, ts1), Func (n2, ts2)  when n1 = n2 ->
     Misc.zipWith (fun x y -> (x, y)) (ts1, ts2) |>
     List.fold_left
       (fun s (t1, t2) ->
          let s' = mgu i (apply_ty s t1) (apply_ty s t2) in
          sub_compose s s'
       ) sub_empty
  | App  (tc1, ts1), App (tc2, ts2) when tc1 = tc2 ->
     Misc.zipWith (fun x y -> (x, y)) (ts1, ts2) |>
     List.fold_left
       (fun s (t1, t2) ->
          let s' = mgu i (apply_ty s t1) (apply_ty s t2) in
          sub_compose s s'
       ) sub_empty

      (* Adding code for polymorphic arguments *)
  | Func (i,[t1]), t2
  | t1, Func (i,[t2]) when i=0 -> mgu i t1 t2
  | t1, t2 -> UnificationError (String.concat " " ("mgu fails on :":: List.map to_string [t1; t2]))
              |> raise


let unifiable t1 t2 =
  let _ = logf ("unifiable " ^ to_string t1 ^ " and " ^ to_string t2) in
  try mgu 0 t1 t2; true with UnificationError _ -> false


let ti_loc f = function
  | Loc s  -> f (Symbol.of_string s)
  | Lvar _ -> None
  | LFun   -> None












let sortcheck_loc f = function
  | Loc s  -> f (Symbol.of_string s)
  | Lvar _ -> None
  | LFun   -> None

let uf_arity f uf =
  match f uf with None -> None | Some t ->
    match func_of_t t with None -> None | Some (i,_,_) ->
      Some i

let solved_app f uf = function
  | Some (s, t) -> begin match uf_arity f uf with
                     | Some n -> if check_arity n s then Some t else None
                     | _      -> None
                   end
  | None        -> None

let checkArity f uf = function
  | None        -> None
  | Some (s, t) -> begin match uf_arity f uf with
                         | Some n -> if check_arity n s then Some (s, t) else None
                         | _      -> None
                   end


let unifiable t1 t2 =
  match unify [t1] [t2] with
  | Some _ -> true
  | _      -> false
