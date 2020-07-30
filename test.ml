type atype = 
  | Val of (atype -> unit)

let loop () = 
  let foo at = 
    match at with
      | Val a -> a at in
  foo (Val foo)

let loop2 () =
  let l = ref (fun a -> ()) in
  l := (fun a -> !l a);
  !l ()
  
type atype2 = 
  | Val2 of (atype2 -> int -> int)
let fib2 n = 
  let foo at n = 
    match at with
      | Val2 a -> 
        match n with 
          | 0 -> 0
          | 1 -> 1
          | _ -> a at (n - 1) + a at (n - 2) in
  foo (Val2 foo) n
let fib3 n = 
  let l = ref (fun a n -> 0) in
  l := (fun a n -> 
    match n with
      | 0 -> 0
      | 1 -> 1
      | _ -> !l a (n - 1) + !l a (n - 2));
  !l () n

let fib_tester fib = 
  [
    fib 5 = 5;
    fib 8 = 21
  ]

let rec fib n = 
  match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib (n - 1) + fib (n - 2)

(* Tail recursion *)
let rec fib_tail_helper n1 n2 n =
  match n with
    | 0 -> n1
    | _ -> fib_tail_helper n2 (n1 + n2) (n - 1)
let fib_tail n = fib_tail_helper 0 1 n

(* Continuation passing style *)
let rec fib_cps_helper n k = 
  match n with
    | 0 -> k 0
    | 1 -> k 1
    | _ -> fib_cps_helper (n - 1) 
      (fun n1 -> fib_cps_helper (n - 2) 
      (fun n2 -> k (n1 + n2)))
let fib_cps n = fib_cps_helper n (fun x -> x)

(* Tail recursive CPS *)
let rec fib_tail_cps_helper n1 n2 n k =
  match n with
    | 0 -> k n1
    | _ -> fib_tail_cps_helper n2 (n1 + n2) (n - 1) k
let fib_tail_cps n = fib_tail_cps_helper 0 1 n (fun a -> a)








