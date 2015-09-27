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

module Make (X : sig type t
                         val sub_equal : t -> t -> bool
                         val hash : t -> int end) = struct

  module HashStruct = struct
    type t = X.t * int
    let equal (x,_) (y,_) = X.sub_equal x y
    let hash (x,_) = X.hash x
  end

  module Hash = Weak.Make(HashStruct)

  let wrap =
    let tab = Hash.create 251 in
    let ctr = ref 0 in
    fun e ->
      let res = Hash.merge tab (e, !ctr) in
      let _   = if snd res = !ctr then incr ctr in
      res

  let unwrap (e,_) = e

end
