(* Create a generic exception here in case we have an empty list *)
exception Empty_list
exception Not_enough_items

(*
   Write a function last : 'a list -> 'a option that returns the last element of a list
*)
let rec last l =
  match l with
  | [] -> raise Empty_list
  | [ x ] -> x
  | _ :: t -> last t
;;

let%test _ = last [ 1; 2; 3 ] = 3

let%test _ =
  try last [] with
  | Empty_list -> true
  | _ -> false
;;

(* Find the last two elements of the list *)
let rec last_two l =
  match l with
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: t -> last_two t
;;

let%test _ = last_two [ 1; 2; 2; 3 ] = Some (2, 3)
let%test _ = last_two [] = None
let%test _ = last_two [ 1 ] = None

(* Find the nth element of the list *)
let rec at n l =
  match l with
  | [] -> None
  | h :: t -> if n = 0 then Some h else at (n - 1) t
;;

let%test _ = at 2 [ 1; 2; 3 ] = Some 3
let%test _ = at 100 [ 1; 2; 3 ] = None

(* Find the length of a list *)
let length l =
  let rec helper l acc =
    match l with
    | [] -> acc
    | _ :: t -> helper t (acc + 1)
  in
  helper l 0
;;

let%test _ = length [ 1; 2; 3; 4; 5 ] = 5

(* Reverse a list *)
let rev l =
  let rec helper l acc =
    match l with
    | [] -> acc
    | h :: t -> helper t (h :: acc)
  in
  helper l []
;;

let%test _ = rev [ 1; 2; 3 ] = [ 3; 2; 1 ]

(* Check if the item is a palindrome *)
let palindrome l =
  let rec helper a b =
    match a, b with
    | [], [] -> true
    | _, [] | [], _ -> false
    | ah :: at, bh :: bt -> if ah = bh then helper at bt else false
  in
  helper l (rev l)
;;

let%test _ = palindrome [ 1; 2; 1 ]
let%test _ = palindrome [ 1 ]
let%test _ = not (palindrome [ 1; 2 ])
