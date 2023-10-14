(* Find the last element of a list. (easy) *)
let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

(* Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t
;;

(* Find the k'th element of a list. (easy) *)
let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k - 1) t
;;

(* Find the number of elements of a list. (easy) *)
let length l =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in
  aux 0 l
;;

(* Reverse a list. (easy) *)
let rev list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] list
;;

(* Find out whether a list is a palindrome (easy) *)
let is_palindrome list = list = List.rev list

(* Flatten a list structure (medium) *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] list)
;;

(* Eliminate consecutive duplicates of list elements. (medium) *)
let compress list =
  let rec aux acc = function
    | [] -> acc
    | [ x ] -> x :: acc
    | a :: (b :: _ as t) -> if a = b then aux acc t else aux (a :: acc) t
  in
  List.rev (aux [] list)
;;

(* Unit Tests *)
let () =
  let _ = assert (last [ "a"; "b"; "c"; "d" ] = Some "d") in
  let _ = assert (last [] = None) in
  let _ = assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")) in
  let _ = assert (last_two [ "a" ] = None) in
  let _ = assert (last_two [] = None) in
  let _ = assert (at 3 [ "a"; "b"; "c"; "d"; "e" ] = Some "c") in
  let _ = assert (at 3 [ "a" ] = None) in
  let _ = assert (at 3 [] = None) in
  let _ = assert (length [ "a"; "b"; "c" ] = 3) in
  let _ = assert (length [] = 0) in
  let _ = assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]) in
  let _ = assert (rev [] = []) in
  let _ = assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ] = true) in
  let _ = assert (is_palindrome [ "a"; "b"; "c"; "d" ] = false) in
  let _ =
    assert (
      flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
      = [ "a"; "b"; "c"; "d"; "e" ])
  in
  print_endline "All Tests Passed"
in
()
