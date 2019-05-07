let null xs = match xs with
  | [] -> true
  | h :: t -> false
in
null (undefined :: [])
;;
