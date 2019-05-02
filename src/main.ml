open Syntax

let srcfile = ref "-"
let strict_eval = ref false

let rec repl prompt chan k =
  print_string prompt;
  flush stdout;

  let exp = Parser.main Lexer.main (Lexing.from_channel chan) in
  print_endline (string_of_exp exp);

  begin
    if !strict_eval then
      let initial_env = Env.empty in
      let result = Eval.eval initial_env exp (fun x -> x) in
      print_endline (Eval.string_of_value result)
    else
      failwith "Sorry, not implemented."
  end;

  k ()

let main () =
  let usage = Printf.sprintf "%s [--use-strict] [filename]" Sys.argv.(0) in
  let arg = Arg.align [
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
