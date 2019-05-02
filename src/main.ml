open Syntax

let rec repl () =
  print_string "# ";
  flush stdout;

  let exp = Parser.main Lexer.main (Lexing.from_channel stdin) in
  print_endline (string_of_exp exp);

  let initial_env = Env.empty in
  let result = Eval.eval initial_env exp (fun x -> x) in
  print_endline (Eval.string_of_value result);

  repl ()

let () = repl ()
