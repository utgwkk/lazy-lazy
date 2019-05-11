let null xs = match xs with
  | [] -> true
  | _ -> false
in
null (undefined :: [])
;;
