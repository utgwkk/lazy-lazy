open Syntax

let srcfile = ref "-"
let verbose = ref false
let strict_eval = ref false

let if_verbose f = if !verbose then f ()

let rec repl prompt chan k =
  print_string prompt;
  flush stdout;

  let exp = Parser.main Lexer.main (Lexing.from_channel chan) in
  if_verbose (fun () -> print_endline (string_of_exp exp));

  let ty = Infer.start exp in
  print_endline (string_of_ty ty);

  let initial_env = Env.empty in

  begin
    if !strict_eval then
      let result = Eval.eval initial_env exp (fun x -> x) in
      print_endline (Eval.string_of_value result)
    else
      let result = LazyEval.eval initial_env exp (fun t ->
        LazyEval.force t (fun v -> v)
      ) in
      print_endline (LazyEval.string_of_value result)
  end;

  k ()

let main () =
  let usage = Printf.sprintf "%s [--use-strict] [-v] [filename]" Sys.argv.(0) in
  let arg = Arg.align [
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
