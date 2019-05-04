let rec map f xs = match xs with
  | [] -> []
  | h :: t -> f h :: map f t
in
let eq m n =
  if m < n then false
  else if n < m then false
  else true
in
let rec divmod m n =
  if m < n then m
  else divmod (m + n * -1) n
in
let rec nat = 0 :: map (fun x -> x + 1) nat
in
let rec filter f xs = match xs with
  | [] -> []
  | h :: t ->
      if f h then h :: filter f t
      else filter f t
in
let rec sift n xs = match xs with
  | [] -> []
  | h :: t ->
      if eq (divmod h n) 0 then sift n t
      else h :: sift n t
in
let rec sieve xs = match xs with
  | [] -> []
  | h :: t -> h :: sieve (sift h t)
in
let rec nth n xs = match xs with
  | [] -> undefined
  | h :: t ->
      if n < 2 then h
      else nth (n + -1) t
in
let greater_than n xs = filter (fun m -> n < m) xs in
let primes = sieve (greater_than 1 nat) in
nth 100 primes
;;
