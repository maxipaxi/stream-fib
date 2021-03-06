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


(*
  ( 1  2 )
  ( 3  4 )
*)
type matrix2x2 = Mat of int * int * int * int

let m1 = Mat (1, 2, 3, 4)
let m2 = Mat (2, 0, 1, 2)

let mat_mult mat1 mat2 = 
  match mat1, mat2 with 
    | Mat (m1, m2, m3, m4), Mat (n1, n2, n3, n4) ->
      Mat (m1 * n1 + m2 * n3, m1 * n2 + m2 * n4
         , m3 * n1 + m4 * n3, m3 * n2 + m4 * n4)

(* Time: O(n) *)
let rec mat_expo m n =
  match n with  
    | 0 -> Mat (1, 0, 0, 1)
    | _ -> mat_mult m (mat_expo m (n - 1))

(*
n ^ a * n ^ b = n ^ (a + b)

m ^ n = m ^ (n/2 + n/2)
      = m ^ n/2 * m ^ n/2
*)
(* Time: O(lg(n)) *)
let rec mat_expo_fast m n =
  match n with
    | 0 -> Mat (1, 0, 0, 1)
    | _ when n mod 2 = 0 -> 
      let half = mat_expo_fast m (n / 2) in
      mat_mult half half
    | _ -> 
      mat_mult m (mat_expo_fast m (n - 1))

let fib_mat = Mat (1, 1
                 , 1, 0)
let fib_log n = 
  match mat_expo_fast fib_mat n with
    | Mat (_, _, n, _) -> n
    



type int_tree = 
  | Leaf of int
  | Node of int_tree * int_tree

let t1 = Node (
  Node (Leaf 0, Leaf 1), 
  Node (Leaf 2, Leaf 3))
let t2 = Node (
  Node (Leaf 1, Leaf 1), 
  Node (Leaf 2, Leaf 3))
      
let mult_leaves_tester mult = 
  [
    mult t1 = 0;
    mult t2 = 6
  ]
  

let rec mult_leaves t =
  match t with
    | Leaf i -> i
    | Node (t1, t2) ->
      let p1 = mult_leaves t1 in
      let p2 = mult_leaves t2 in
      p1 * p2

let rec mult_leaves_cps_helper t k =
  match t with
    | Leaf i -> 
      print_endline "Leaf";
      k i
    | Node (t1, t2) ->
      mult_leaves_cps_helper t1 (fun p1 -> 
      mult_leaves_cps_helper t2 (fun p2 -> 
      k (p1 * p2)))
let mult_leaves_cps t = mult_leaves_cps_helper t (fun i -> i)

let rec mult_leaves_cps_opt_helper t k =
  match t with
    | Leaf 0 -> 0
    | Leaf i -> 
      print_endline "Leaf";
      k i
    | Node (t1, t2) ->
      mult_leaves_cps_opt_helper t1 (fun p1 -> 
      mult_leaves_cps_opt_helper t2 (fun p2 -> 
      k (p1 * p2)))
let mult_leaves_cps_opt t = mult_leaves_cps_opt_helper t (fun i -> i)


let rec find_min t = 
  match t with
    | Leaf n -> n
    | Node (t1, t2) -> 
      let n1 = find_min t1 in
      let n2 = find_min t2 in
      min n1 n2
let rec replace_leaves t n =
  match t with
    | Leaf _ -> Leaf n
    | Node (t1, t2) -> 
      Node (replace_leaves t1 n
          , replace_leaves t2 n)

(* Time ~ 2n = O(n) *)
let replace_min t = replace_leaves t (find_min t)

(* Circular programming - Richard Bird *)
(* Time ~ n = O(n) *)
let rec replace_leaves_onepass_helper t =
  match t with
    | Leaf n -> n, fun n -> Leaf n
    | Node (t1, t2) -> 
      let n1, f1 = replace_leaves_onepass_helper t1 in
      let n2, f2 = replace_leaves_onepass_helper t2 in
      min n1 n2, fun n -> Node (f1 n, f2 n)
let replace_leaves_onepass t = 
  let n, f = replace_leaves_onepass_helper t in
  f n

type 'a lst = 
  | Nil
  | Cons of 'a * 'a lst

let xs = Cons (1, Cons (2, Cons (3, Nil)))
let ys = Cons (4, Cons (5, Cons (6, Nil)))

let rec append xs ys =
  match xs with
    | Nil -> ys
    | Cons (x, xs') -> Cons (x, append xs' ys)

let rec snoC xs x =
  match xs with
    | Nil -> Cons (x, Nil)
    | Cons (x', xs') -> Cons (x', snoC xs' x) 

let rec rev xs =
  match xs with
    | Nil -> Nil
    | Cons (x, xs) -> snoC (rev xs) x

let rec rev_tail_helper xs acc =
  match xs with
    | Nil -> acc
    | Cons (x, xs') -> rev_tail_helper xs' (Cons (x, acc))
let rev_tail xs = rev_tail_helper xs Nil

(* Time ~ 2n = O(n) *)
let reverse_append xs ys = rev_tail (append xs ys)
let reverse_append2 xs ys = append (rev_tail ys) (rev_tail xs)

(* Time ~ n = O(n) *)
let reverse_append3 xs ys = rev_tail_helper ys (rev_tail_helper xs Nil)

let flip f a b = f b a

let rec length xs =
  match xs with 
    | Nil -> 0
    | Cons (x, xs') -> 1 + length xs'
let rec sum xs = 
  match xs with 
    | Nil -> 0
    | Cons (x, xs') -> x + sum xs'
let rec subtract n xs = 
  match xs with 
    | Nil -> Nil
    | Cons (x, xs') -> Cons (x - n, subtract n xs')

(* Time ~ 3n = O(n) *)
let subtract_avg xs = subtract (sum xs / length xs) xs

(* Time ~ n = O(n) *)
let rec sub_avg_helper xs =
  match xs with
    | Nil -> 0, 0, fun n -> Nil
    | Cons (x, xs') -> 
      let (length, sum, constructor) = sub_avg_helper xs' in
      1 + length, x + sum, fun n -> Cons (x - n, constructor n)
let sub_avg xs =
  let (length, sum, constructor) = sub_avg_helper xs in
  constructor (sum / length)

(*
Convolution
  [1, 2, 3] [4, 5, 6]
  [(1, 6), (2, 5), (3, 4)]
*)

let rec zip xs ys =
  match xs, ys with
    | Nil, _ -> Nil
    | _, Nil -> Nil
    | Cons (x, xs'), Cons (y, ys') ->
      Cons ((x, y), zip xs' ys')

(* Time ~ 2n = O(n) *)
let convolution xs ys = zip xs (rev_tail ys)

(* There and back again -- Olivier Danvy, Mayer Goldberg *)
(* Time ~ n = O(n) *)
let rec conv xs ys =
  match xs with
    | Nil -> Nil, ys
    | Cons (x, xs') ->
      let (xsr, ys) = conv xs' ys in
      match ys with
        | Nil -> failwith "Impossible"
        | Cons (y, ys') -> 
          Cons ((x, y), xsr), ys'

let palin = Cons (1, Cons (2, Cons (2, Cons (1, Nil))))

(* Time ~ 2n = O(n) *)
let rec is_palindrome_helper xs rev_xs =
  match xs, rev_xs with
    | Nil, Nil -> true
    | Cons (x, xs'), Cons (x', rev_xs') -> 
      x = x' && is_palindrome_helper xs' rev_xs'
    | _ -> failwith "Impossible"

let is_palindrome xs = is_palindrome_helper xs (rev_tail xs)

(* Time ~ n = O(n) *)
let rec is_palin_helper xs rev_xs =
  match xs with
    | Nil -> true, rev_xs
    | Cons (x, xs') ->
      let palin_so_far, rev_xs = is_palin_helper xs' rev_xs in
      match rev_xs with
        | Nil -> failwith "Impossible"
        | Cons (x', rev_xs') ->
          x = x' && palin_so_far, rev_xs'
let is_palin xs = 
  let (palin_so_far, Nil) = is_palin_helper xs xs in
  palin_so_far

(* Time ~ n/2 = O(n) *)
let rec is_palin_helper2 xs rev_xs =
  match xs with
    | Nil -> true, rev_xs
    | Cons (x, Nil) ->
      begin
        match rev_xs with
          | Cons (x', rev_xs') ->
            x = x', rev_xs'
      end
    | Cons (x1, Cons (x2, xs')) ->
      let palin_so_far, rev_xs = is_palin_helper xs' rev_xs in
      begin
        match rev_xs with
          | Cons (x'1, Cons (x'2, rev_xs')) ->
            x1 = x'2 && x2 = x'1 && palin_so_far, rev_xs'
      end
let is_palin2 xs = 
  let (palin_so_far, _) = is_palin_helper2 xs xs in
  palin_so_far



