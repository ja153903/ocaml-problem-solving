(* Flatten a nested array *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten l =
  match l with
  | [] -> []
  | x ->
    let rec helper lst acc =
      match lst with
      | [] -> acc
      | One x :: t -> helper t (acc @ [ x ])
      | Many lst' :: t -> helper t (acc @ flatten lst')
    in
    helper x []
;;

let%test _ =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]
;;

(* Eliminate duplicates *)
let compress l =
  let rec includes l x =
    match l with
    | [] -> false
    | h :: t -> h = x || includes t x
  in
  let rec helper l aux seen =
    match l with
    | [] -> aux
    | h :: t ->
      if includes seen h then helper t aux seen else helper t (h :: aux) (h :: seen)
  in
  List.rev (helper l [] [])
;;

let%test _ = compress [ 1; 1; 1; 2; 2; 3 ] = [ 1; 2; 3 ]
let%test _ = compress [ 1; 2; 3 ] = [ 1; 2; 3 ]
let%test _ = compress [] = []

(* Pack consecutive duplicates *)
let pack l =
  let rec helper l aux cur prev =
    match l with
    | [] -> if List.length cur <> 0 then cur :: aux else aux
    | h :: t ->
      (match prev with
       | Some x ->
         if x = h
         then helper t aux (h :: cur) (Some h)
         else helper t (cur :: aux) [ h ] (Some h)
       | None -> helper t aux (h :: cur) (Some h))
  in
  List.rev (helper l [] [] None)
;;

let%test _ =
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

type 'a another_node =
  | One of 'a
  | Many of int * 'a

(*
   Decode a run-encoded list
   #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
  - : string list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)

let decode l =
  let repeat ch n =
    let rec helper n aux = if n == 0 then aux else helper (n - 1) (ch :: aux) in
    helper n []
  in
  let rec helper l aux =
    match l with
    | [] -> aux
    | h :: t ->
      (match h with
       | One x -> helper t (x :: aux)
       | Many (count, x) -> helper t (repeat x count @ aux))
  in
  List.rev (helper l [])
;;

let%test _ =
  decode [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
  = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;

(*
   Run-length encoding of a list

  Like the previous problem but in reverse
*)

let encode l =
  let rec map_to_node l aux =
    match l with
    | [] -> aux
    | h :: t ->
      if List.length h > 1
      then map_to_node t (Many (List.length h, List.hd h) :: aux)
      else map_to_node t (One (List.hd h) :: aux)
  in
  List.rev (map_to_node (pack l) [])
;;

let%test _ =
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;

(* Replicate element in list n times *)
let rec replicate l n =
  let rec repeat c n = if n == 0 then [] else c :: repeat c (n - 1) in
  match l with
  | [] -> []
  | h :: t -> repeat h n @ replicate t n
;;

let%test _ =
  replicate [ "a"; "b"; "c" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
;;

(* drop every nth element of a list *)
let drop l n =
  let rec helper l i =
    match l with
    | [] -> []
    | h :: t -> if h mod n = 0 then helper t (i + 1) else h :: helper t (i + 1)
  in
  helper l 1
;;

let%test _ = drop [ 1; 2; 3; 4; 5; 6 ] 3 = [ 1; 2; 4; 5 ]

(* Extract a slice from a list *)
let slice l i j =
  let rec helper l aux idx =
    match l with
    | [] -> aux
    | h :: t ->
      if idx > j
      then aux
      else if idx < i
      then helper t aux (idx + 1)
      else helper t (h :: aux) (idx + 1)
  in
  List.rev (helper l [] 0)
;;

let%test _ =
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
  = [ "c"; "d"; "e"; "f"; "g" ]
;;

(* Rotate a list n places to the left *)
let rotate l n =
  (* we can drop the first n and put them into another list *)
  List.drop n l @ List.take n l
;;

let%test _ =
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
  = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
;;
