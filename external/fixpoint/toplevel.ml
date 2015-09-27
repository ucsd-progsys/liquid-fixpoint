module BS = BNstats
module SM = Symbol.SMap
module Co = Constants
module C  = FixConstraint
module F  = Format
module Misc = FixMisc open Misc.Ops


(*
let parse_string str =
  let lb = Lexing.from_string str in
    ArithParser.aexpr ArithLexer.token lb

let token_list_of_string s =
  let lb = Lexing.from_string s in
  let rec helper l =
    try
      let t = ArithLexer.token lb in
      if t = ArithParser.EOF then List.rev l else helper (t::l)
    with _ -> List.rev l
  in helper []

let eval_string env str = ArithInterpreter.eval env (parse_string str)
*)

(*****************************************************************)
(********************* Command line options **********************)
(*****************************************************************)

let parse f =
  let _  = Errorline.startFile f in
  let ic = open_in f in
  let rv = Lexing.from_channel ic |> FixParse.defs FixLex.token in
  let _  = close_in ic in
  rv

let read_inputs usage =
  Co.bprintflush true "\n\n";
  Co.bprintflush true    "============================================================================\n";
  Co.bprintflush true    "© Copyright 2009-15 Regents of the University of California.\n";
  Co.bprintflush true    "All Rights Reserved.\n";
  Co.bprintflush true    "============================================================================\n";
  Co.bprintflush false   (Sys.argv |> Array.to_list |> String.concat " ");
  Co.bprintflush false "\n============================================================================\n";
  let fs = ref [] in
  let _  = Arg.parse Co.arg_spec (fun s -> fs := s::!fs) usage in
  let fq = !fs |> BS.time "parse" (Misc.flap parse) |> FixConfig.create in
  (!fs, fq)
