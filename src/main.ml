open Syntax

let srcfile = ref "-"
let benchmark = ref false
let verbose = ref false
let strict_eval = ref false

let with_benchmark label f =
  if !benchmark then
    let start_time = Sys.time () in
    let ret = f () in
    let end_time = Sys.time () in
    let dt = end_time -. start_time in
    Printf.printf "%s : elapsed time = %.8f sec\n" label dt;
    ret
  else
    f ()

let if_verbose f = if !verbose then f ()

let rec repl prompt chan k =
  print_string prompt;
  flush stdout;

  let exp = Parser.main Lexer.main (Lexing.from_channel chan) in
  if_verbose (fun () -> print_endline (string_of_exp exp));

  let ty =
    with_benchmark "type inference" (fun () -> Infer.start exp)
  in

  let value_str =
    begin
      if !strict_eval then
        let result =
          with_benchmark "strict eval" (fun () -> Eval.start exp)
        in
        Eval.string_of_value result
      else
        let result =
          with_benchmark "lazy eval" (fun () -> LazyEval.start exp)
        in
        LazyEval.string_of_value result
    end
  in

  (
    if value_str = "undefined" then
      print_endline "Exception: undefined"
    else
      Printf.printf "- : %s = %s\n" (pp_ty ty) value_str
  );

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
