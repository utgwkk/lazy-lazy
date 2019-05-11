let rec iseven n = match n with
  | 0 -> true
  | 1 -> false
  | n ->
      if n < 0 then iseven (n * -1)
      else iseven (n + -2)
in iseven 100
;;
