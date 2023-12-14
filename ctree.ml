open Printf

(* ------------------------ *)
(*    Module des arbres     *)
(* ------------------------ *)


type 'a t_btree =  Vide | B_RT of ('a * 'a t_btree * 'a t_btree)

let bt_empty : 'a t_btree = Vide;;

(*****     ARBRE BINAIRE DE RECHERCHE    *****)

(* 1.1 *)

let bt_rooting x g d : 'a t_btree =
  B_RT(x,g,d)
;;


let bt_isempty a : bool =
  a = Vide
;;

let bt_root a : 'a =
  match a with
    Vide -> failwith (" erreur : l'arbre est vide ")
  |B_RT(v,g,d) -> v
;; 


let bt_subleft a : 'a t_btree =
  match a with
    Vide -> failwith ("erreur : l'arbre est vide ")
  |B_RT(v,g,d)  -> g
;;

let bt_subright a : 'a t_btree =
  match a with
    Vide         -> failwith ("erreur : l'arbre est vide ")
  |B_RT(v,g,d)   -> d
;;

let rec bt_size a : int =
  match a with
    Vide -> 0
  |B_RT (x,g,d) -> 1 + (bt_size g) + (bt_size d)
;;

let rec bt_height a : int =
  match a with
    Vide -> 0;
  |B_RT(x,g,d) -> 1 + max (bt_height g) (bt_height d)
;;



let rec bst_seek a e : bool =
  match a with
    Vide -> true
  |B_RT(v,g,d) ->
      if e = v
      then false
      else
      if e < v
      then (bst_seek g e)
      else (bst_seek d e)
;;

let rec bst_insert a e : 'a t_btree =
  match a with
    Vide -> (bt_rooting e (bt_empty) (bt_empty))
  |B_RT(v,g,d) ->
      if v > e
      then (bt_rooting v (bst_insert g e) d)
      else (bt_rooting v g (bst_insert d e))
;;

let rec delete_min a : 'a t_btree * 'a =
  match a with
    Vide -> failwith "delete_min: Empty tree"
  | B_RT (v, Vide, d) -> (d,v)
  | B_RT (v, g, d) ->
      let (new_left, min_val : 'a t_btree * 'a) = (delete_min g) in
      (B_RT (v, new_left, d), min_val)
;;

let rec bst_delete a e : 'a t_btree =
  match a with
    Vide -> Vide
  |B_RT(v,g,d) ->
      if e = v then
        match g, d with
        | Vide, _ -> d
        | _, Vide -> g
        | _, _ ->
            let (new_right, min_val: 'a t_btree * 'a) = (delete_min d) in
            B_RT(min_val, g, new_right)
      else if e < v
      then B_RT (v, (bst_delete g e), d)
      else B_RT (v, g, (bst_delete d e))
;;

let ab_desequilibre t : int =
  if (bt_isempty t)
  then 0
  else (bt_height (bt_subleft t)) - (bt_height (bt_subright t))
;;

let rec bst_lbuild a : 'a t_btree =
  match a with
    [] -> bt_empty
  |hd :: l -> (bst_insert (bst_lbuild l) hd)
;;

let rec bst_cut a v : 'a t_btree * 'a t_btree =
  if (bt_isempty a)
  then (bt_empty, bt_empty)
  else
    let (r,fg,fd): 'a * 'a t_btree *'a t_btree = ((bt_root a), (bt_subleft a), (bt_subright a)) in
    if (v < r)
    then
      let (g1, d1) : 'a t_btree * 'a t_btree = (bst_cut fg v) in
      (g1,(bt_rooting r d1 fd))
    else
      let (g1, d1) : 'a t_btree * 'a t_btree = (bst_cut fd v) in
      ((bt_rooting r fg g1), d1)
;;

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

(* créer un ABR aléatoire *)
let rec bst_rnd_create(n) : 'a t_btree =
  if n = 0
  then Vide
  else (bst_lbuild (list_rnd_create(n-1)))
;;

let print_val lvl v : unit =
  Printf.printf "LVL %d : %d\n%!" lvl v
;;

let rec see_btree a niveau : unit =
  match a with
  | Vide -> ()
  | B_RT(v, g, d) -> (
    (print_val niveau v);
    (see_btree g (niveau+1));
    (see_btree d (niveau+1));
  )
;;


let rec bst_isBstRed abr value sup : bool =
   match abr with
   | Vide -> true
   | B_RT(v, g, d) -> (
   	let resCmp : bool = (
   	if sup == true
   	then v>value
   	else v<value
        ) in
   	if (resCmp)
   	then false
   	else
   		if not(bst_isBstRed g v false)
   		then false
   		else (bst_isBstRed d v true)
   );;

let bst_isBst abr : bool =
   	match abr with
   	| Vide -> true
   	| B_RT(v, g, d) -> (
   		(bst_isBstRed g v false) && (bst_isBstRed d v true)
   	);;
   	
   	(*******      ARBRE AVL     *********)

(* 2.1.1 *)

(* Rotations à partir des axiomes et des exemples fournis *)

(* Rotation gauche *)
let ab_rg( a : 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("Erreur")
  |B_RT(_,_,Vide) -> failwith ("Erreur")
  |B_RT(p, u, B_RT(q,v,w)) -> B_RT(q, B_RT(p,u,v), w)
;;

(* Rotaiton droite *)
let ab_rd( a : 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("Erreur")
  |B_RT(_, Vide, _) -> failwith ("Erreur")
  |B_RT(q, B_RT(p,u,v), w) -> B_RT(p, u, B_RT(q,v,w))
;;

(* Rotaition gauche droit *)
let ab_rgd( a : 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("Erreur")
  |B_RT(_,Vide,_) -> failwith ("Erreur")
  |B_RT(_,B_RT(_,_,Vide),_) -> failwith ("Erreur")
  |B_RT(r, B_RT(p,t,B_RT(q,u,v)),w) -> B_RT(q, B_RT(p,t,u), B_RT(r,v,w))
;;

(* Rotaiton droite gauche *)
let ab_rdg( a : 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("Erreur")
  |B_RT(_,_,Vide) -> failwith ("Erreur")
  |B_RT(_,_,B_RT(_,Vide,_)) -> failwith ("Erreur")
  |B_RT(r, t, B_RT(p,B_RT(q,u,v),w)) -> B_RT(q, B_RT(r,t,u), B_RT(p,v,w))
;;
