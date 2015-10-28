
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

module F    = Format
module Misc = FixMisc
open Misc.Ops
module SM = Misc.StringMap
module IS = Misc.IntSet

let mydebug = false

type t = string

let mk_wild =
      let t,_ = Misc.mk_int_factory () in
      t <+> string_of_int <+> (^) "~A"

let is_wild_fresh s = s = "_"
let is_wild_any   s = s.[0] = '~'
let is_wild_pre   s = s.[0] = '@'
let is_wild s       = is_wild_fresh s || is_wild_any s || is_wild_pre s

let is_safe s =
      let re = Str.regexp "[A-Za-z '~' '_' '\'' '@' ][0-9 a-z A-Z '_' '$' '@' '\'' '.' '#']*$" in
      Str.string_match re s 0

let to_string_raw x = x

let of_string, to_string =
  let of_t = Hashtbl.create 117 in
  let to_t = Hashtbl.create 117 in
  let bind = fun s sy -> Hashtbl.replace of_t s sy; Hashtbl.replace to_t sy s in
  let f,_  = Misc.mk_string_factory "FIXPOINTSYMBOL_" in
  ((fun s ->
        if is_wild_fresh s then mk_wild () else
        if is_safe s then s else
           try Hashtbl.find of_t s with Not_found ->
             let sy = f () in
             let _  = bind s sy in sy),
    (fun sy -> try Hashtbl.find to_t sy with Not_found -> sy))

let to_string = fun s -> s (* if is_safe s then s else "'" ^ s ^ "'" *)

let suffix = fun s suff -> of_string ((to_string s) ^ suff)

let print fmt s =
      to_string s |> Format.fprintf fmt "%s"

(* DEAD
let vvprefix = "VV_"

let vvsuffix = function
      | Sort.Ptr l -> Sort.loc_to_string l
      | t          -> Sort.to_string_short t

let value_variable t = vvprefix ^ (vvsuffix t)


let is_value_variable = Misc.is_prefix vvprefix
*)

(* DEBUG *)
let vvprefix = "VV"
let is_value_variable = (=) vvprefix
let value_variable _  = vvprefix

module SMap = Misc.EMap (struct
  type t = string
  let compare i1 i2 = compare i1 i2
  let print         = print
  end)

module SSet = Misc.ESet (struct
  type t = string
  let compare i1 i2 = compare i1 i2
  end)
