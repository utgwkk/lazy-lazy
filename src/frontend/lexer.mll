{
  open Parser

  let reservedWords = [
    ("else", ELSE);
    ("false", FALSE);
    ("fun", FUN);
    ("if", IF);
    ("in", IN);
    ("let", LET);
    ("match", MATCH);
    ("rec", REC);
    ("then", THEN);
    ("true", TRUE);
    ("undefined", UNDEFINED);
    ("with", WITH);
  ] |> List.sort compare
}

let small = ['a'-'z']
let capital = ['A'-'Z']
let digit = ['0'-'9']
let number = digit+
let ident = '_' | small (capital | small | digit | ['_' '\''])*

rule main = parse
  | [' ' '\009' '\012' '\r' '\t' '\n']+ { main lexbuf }
  | "-"? number { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";;" { SEMISEMI }
  | "->" { RARROW }
  | "[" { LLPAREN }
  | "]" { RLPAREN }
  | ";" { SEMI }
  | "," { COMMA }
  | "::" { CONS }
  | "|" { PIPE }
  | "=" { EQ }
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
  | "(*" { comment 0 lexbuf }
	| eof { exit 0 }
  | _ as x { failwith ("unknown char: " ^ (String.make 1 x))}

and comment nest = parse
  | "*)" {
      if nest < 1 then main lexbuf
      else comment (nest - 1) lexbuf
    }
  | "(*" { comment (nest + 1) lexbuf }
  | _ { comment nest lexbuf }
