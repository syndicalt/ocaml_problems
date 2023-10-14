(* Append new item of same type to the end of a list. (easy) *)
let rec append_last list = function
  | [] -> [ list ]
  | h :: t -> h :: append_last list t

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

(* Pack consecutive duplicates of list elements into sublists. (medium) *)
let pack list =
  let rec aux acc = function
    | [] -> []
    | [ x ] -> (x :: acc) :: []
    | a :: (b :: _ as t) ->
      if a = b then aux (a :: acc) t else (a :: acc) :: aux [] t
  in
  aux [] list
;;
(* Run-length encoding of a list. (easy) *)
let encode list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)
;;

(* Modified run-length encoding. (medium) *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
let encode_rle list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> if count = 0 then One x :: acc else Many (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t
      else if count = 0 then aux 0 (One a :: acc) t
      else aux 0 (Many (count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)
;;

(* Decode a run-length encoded list. (medium) *)
let decode_rle list = 
  let rec many acc n x = 
    if n = 0 then acc else many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
  aux [] (List.rev list)
;;

(* Duplicate elements of a list *)
let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in
  List.rev (aux [] list)
;;
  
(* Replicate the elements of a list a given number of times *)
let repl_list list n =
  let rec many acc n x =
    if n = 0 
    then acc 
    else many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (many acc n h) t
  in
  List.rev (aux [] list)
;;

let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node ('x', l, r) :: a) all right in
  List.fold_left add_right_tree all left

let rec cbal_tree n =
  if n = 0 then [Empty]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    add_trees_with t t []
  else (* n even: n-1 nodes for the left & right subtrees altogether. *)
    let t1 = cbal_tree (n / 2 - 1) in
    let t2 = cbal_tree (n / 2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 [])
  ;;

(* Unit Tests *)
let () =
  (* append_last *)
  let _ = assert (append_last "d" [ "a"; "b"; "c" ] = [ "a"; "b"; "c"; "d" ]) in
  let _ = assert (append_last "d" [] = [ "d" ]) in
  (* last *)
  let _ = assert (last [ "a"; "b"; "c"; "d" ] = Some "d") in
  let _ = assert (last [] = None) in
  (* last_two *)
  let _ = assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")) in
  let _ = assert (last_two [ "a" ] = None) in
  let _ = assert (last_two [] = None) in
  (* at 3 *)
  let _ = assert (at 3 [ "a"; "b"; "c"; "d"; "e" ] = Some "c") in
  let _ = assert (at 3 [ "a" ] = None) in
  let _ = assert (at 3 [] = None) in
  (* length *)
  let _ = assert (length [ "a"; "b"; "c" ] = 3) in
  let _ = assert (length [] = 0) in
  (* rev *)
  let _ = assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]) in
  let _ = assert (rev [] = []) in
  (* is_palindrome *)
  let _ = assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ] = true) in
  let _ = assert (is_palindrome [ "a"; "b"; "c"; "d" ] = false) in
  (* flatten *)
  let _ =
    assert (
      flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
      = [ "a"; "b"; "c"; "d"; "e" ])
  in
  (* compress *)
  let _ =
    assert (
      compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e";
                 "e"; "e" ]
      = [ "a"; "b"; "c"; "a"; "d"; "e" ])
  in
  let _ = assert (compress [ "a" ] = [ "a" ]) in
  let _ = assert (compress [] = []) in
  (* pack *)
  let _ = assert(pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e";
                        "e"; "e"; "e" ]
                  = [ [ "a"; "a"; "a"; "a" ]; [ "b" ]; [ "c"; "c" ]; [ "a"; "a" ];
                      [ "d" ]; [ "e"; "e"; "e"; "e" ] ])
  in
  let _ = assert(pack [ "a" ] = [ [ "a" ] ]) in
  let _ = assert(pack [] = []) in
  (* encode *)
  let _ = assert(encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e";
                          "e"; "e"; "e" ]
                  = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ])
  in
  let _ = assert(encode [ "a" ] = [ (1, "a") ]) in
  let _ = assert(encode [] = []) in
  (* encode_rle *)
  let _ = assert(encode_rle [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d";
                              "e"; "e"; "e"; "e" ]
                  = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a");
                      One "d"; Many (4, "e") ])
  in
  let _ = assert(encode_rle [ "a" ] = [ One "a" ]) in
  let _ = assert(encode_rle [] = []) in
  (* decode_rle *)
  let _ = assert(decode_rle [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) in
  let _ = assert(decode_rle [One "a"] = ["a"]) in
  let _ = assert(decode_rle [Many (4, "a")] = ["a"; "a"; "a"; "a"]) in
  let _ = assert(decode_rle [] = []) in
  print_endline "All Tests Passed"
in
()
;;