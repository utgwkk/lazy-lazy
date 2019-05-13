open Syntax

let srcfile = ref "-"
let benchmark = ref false
let verbose = ref false
let strict_eval = ref false

let if_verbose f = if !verbose then f ()

let do_benchmark exp =
  let iteration = 100000L in
  ignore ([
    Benchmark.latency1 iteration ~name:"type_inference" Infer.start exp;
    (
      if !strict_eval then
        Benchmark.latency1 iteration ~name:"strict_evaluation" StrictEval.start exp
      else
        Benchmark.latency1 iteration ~name:"lazy_evaluation" LazyEval.start exp
    );
  ])

let rec repl prompt chan k =
  print_string prompt;
  flush stdout;

  let exp = Parser.main Lexer.main (Lexing.from_channel chan) in
  if_verbose (fun () -> print_endline (string_of_exp exp));

  if !benchmark then do_benchmark exp;

  try
    Sys.catch_break true;
    let ty = Infer.start exp in

    let value_str =
      if !strict_eval then
        let result = StrictEval.start exp in
        StrictEval.string_of_value result
      else
        let result = LazyEval.start exp in
        LazyEval.string_of_value result
    in

    (
      if value_str = "undefined" then
        print_endline "Exception: undefined"
      else
        Printf.printf "- : %s = %s\n" (pp_ty ty) value_str
    );
    Sys.catch_break false;
    k ()
  with
    Sys.Break ->
      print_endline "Interrupted.";
      Sys.catch_break false;
      k ()


let main () =
  let usage = Printf.sprintf "%s [--use-strict] [-v] [filename]" Sys.argv.(0) in
  let arg = Arg.align [
    ("--benchmark", Arg.Set benchmark, "Do a benchmark.");
    ("-v", Arg.Set verbose, "Verbose mode.");
    ("--use-strict", Arg.Set strict_eval, "Use strict evaluation strategy.");
  ] in
  Arg.parse arg (fun s -> srcfile := s) usage;

  if !srcfile = "-" then
    let chan = stdin in
    let rec k () = repl "# " chan k in
    repl "# " chan k
  else
    let chan = open_in !srcfile in
    let rec k () = close_in chan in
    repl "" chan k

let () = main ()
