(* ------------------------ *)
(*        listes            *)
(* ------------------------ *)

open Printf

let len l : int = List.length l ;;

let empty : 'a list =
  []
;;

let isempty l : bool =
  l = []
;;

let fst l : 'a =
  match l with
    [] -> failwith "error fst : list is empty"
  | hd::_ -> hd
;;

let rec lst l : 'a =
  match l with
    [] -> failwith "error lst : list is empty"
  | hd::[] -> hd
  | _::tail -> lst tail
;;

let nth l k : 'a = 
  let rec nth1(l, k) =
    match l with
      []->  failwith "error nth : index out of bounds"
    | hd::tail -> if k = 0 then hd else nth1(tail,k-1)
  in
  if k < 0
  then failwith "error  nth : index must be positive"
  else nth1(l,k)
;;

let add_fst l e : 'a list = e::l ;;

let rec add_lst l e : 'a list =
  match l with
    [] -> [e]
  | hd::tail -> hd::(add_lst tail e)
;;

let add_nth l e k : 'a list =
  let rec add_nth1(l, e, k)  =
    match l with
      [] -> [e]
    | hd ::tail -> if k = 0 then e::l else hd::add_nth1(tail, e, k-1)
  in
  if k < 0
  then failwith "error add_nth : index must be positive"
  else
  if k > (len l)
  then failwith "error add_nth : index out of bounds"
  else add_nth1(l, e, k)
;;

let rem_fst l : 'a list =
  match l with
    [] -> failwith "error rem_fst : list is empty"
  | _::tail -> tail
;;

let rec rem_lst l : 'a list =
  match l with
    [] -> failwith "error rem_lst : list is empty"
  | [x] -> []
  | x::tail -> x::(rem_lst tail)
;;

let rem_nth l k : 'a list =
  let rec rem_nth1(l, k) =
    match l with
    | [] -> failwith "error rem_nth : index out of bounds"
    | hd:: tail -> if k = 0 then tail else hd::rem_nth1(tail, k-1)
  in
  if k < 0
  then failwith "error rem_nth : index must be positive"
  else rem_nth1(l, k)
;;

let concat l1 l2 = l1 @ l2 ;;

let rec list_rnd_create n : int list =
  Random.self_init();
  let count : int ref = ref n in
  let res : int list ref = ref [] in
  while (!count) > 0 do(
  	res := [Random.int(100)] @ (!res);
  )
  done;
  !res
;;
