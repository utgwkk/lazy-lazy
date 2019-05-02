{
  open Parser

  let reservedWords = [
    ("else", ELSE);
    ("false", FALSE);
    ("if", IF);
    ("then", THEN);
    ("true", TRUE);
  ] |> List.sort compare
}

let small = ['a'-'z']
let capital = ['A'-'Z']
let digit = ['0'-'9']
let number = digit+
let ident = small (capital | small | digit | ['_' '\''])*

rule main = parse
  | [' ' '\009' '\012' '\n']+     { main lexbuf }
  | "-"? number { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";;" { SEMISEMI }
  | "+" { PLUS }
  | "*" { MULT }
  | "<" { LT }
  | ident
    {
      let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
    }
	| eof { exit 0 }
