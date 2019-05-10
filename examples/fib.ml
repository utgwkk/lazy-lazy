let rec take = fun n -> fun xs -> match xs with
    [] -> []
  | hd :: tl -> if n < 1 then []
      else hd :: take (n + -1) tl
in
let tl = fun xs -> match xs with
    [] -> []
  | hd :: tl -> tl
in
let rec zipwith = fun f -> fun xs -> fun ys -> match xs with
    [] -> []
  | xhd :: xtl -> match ys with
      [] -> []
    | yhd :: ytl -> f xhd yhd :: zipwith f xtl ytl
in
let rec fib = 0 :: 1 :: zipwith (+) fib (tl fib) in
take 20 fib
;;
