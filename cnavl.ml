(* Module des arbres AVL avec le désquilibre d’un arbre est stocké à la racine de cet arbre *)

(* Chaque nœud dans la structure Node stocke non seulement la valeur, les sous-arbres gauche et droit, mais aussi la hauteur de ce nœud. La hauteur est mise à jour à chaque modification de l'arbre (insertion ou suppression), et le déséquilibre est calculé en fonction de ces hauteurs. *)

type 'a avl_tree =
  | Empty
  | Node of 'a * 'a avl_tree * 'a avl_tree * int
;;

let avl_height t : int =
  match t with 
  | Empty -> 0
  | Node (v, g, d, h) -> h
;;

let avl_update_height t : 'a avl_tree =
  match t with
  | Node (v, g, d, _) ->
    let left_height : int = (avl_height g) in
    let right_height : int = (avl_height d) in
    Node (v, g, d, 1 + max left_height right_height)
  | Empty -> Empty
;;

let avl_desequilibre t : int =
  match t with
  | Empty -> 0
  | Node (v, g, d, h) -> (avl_height g) - (avl_height d)
;;

let avl_rg t : 'a avl_tree =
  match t with
    Empty -> failwith ("Erreur")
  |Node(_,_,Empty,_) -> failwith ("Erreur")
  |Node(p, u, Node(q,v,w,_),_) -> Node(q, Node(p,u,v,0), w,0)
;;

let avl_rd t : 'a avl_tree =
   match t with
   Empty -> failwith ("Erreur")
  |Node(_, Empty, _,_) -> failwith ("Erreur")
  |Node(q, Node(p,u,v,_), w,_) -> Node(p, u, Node(q,v,w,0),0)
;;

let avl_rgd t : 'a avl_tree =
    match t with
      Empty -> failwith ("Erreur")
     |Node(v,g,d,_) ->
       let rg: 'a avl_tree = (avl_rg g) in (avl_rd (Node(v, rg, d, 0)))
;;

let avl_rdg t : 'a avl_tree =
  match t with
    Empty -> failwith ("Erreur")
   |Node(v, g, d,_) ->
     let rd: 'a avl_tree = (avl_rd d) in (avl_rg (Node(v, g, rd, 0)))
;;

let rec avl_reequilibre t : 'a avl_tree =
  match t with
  |Node(v, g, d, h) ->
    let desequi = (avl_desequilibre t) in
    if desequi > 1
    then
      if (avl_desequilibre g) < 0
      then (avl_rgd t)
      else (avl_rd t)
    else
      if desequi == -1
      then
        if (avl_desequilibre d) > 0
        then (avl_rdg t)
        else (avl_rg t)
      else t
  | _ -> t
;;


(* 2.1.3 *)

let rec avl_inserer t e : 'a avl_tree =
  match t with
    Empty -> Node(e, Empty, Empty, 1)
   |Node(v, g, d, h) ->
     if e < v
     then (avl_reequilibre (avl_update_height (Node(v, (avl_inserer g e),d,h))))
     else
       if e > v
       then (avl_reequilibre (avl_update_height (Node(v,g, (avl_inserer d e), h))))
       else Node(v,g,d,h)
;;

let rec find_min t : 'a =
  match t with
    Empty -> failwith ("Erreur")
   |Node(v, Empty, _,_) -> v
   |Node(v, g,_,_) -> (find_min g)
;;

let rec avl_supprimer t e : 'a avl_tree =
  match t with
    Empty -> Empty
   |Node(v, g, d,_) ->
     if e < v
     then
       let new_l = (avl_supprimer g e) in (avl_reequilibre (avl_update_height (Node(v, new_l, d, 0))))
     else
       if e > v
       then
         let new_l = (avl_supprimer d e) in (avl_reequilibre (avl_update_height (Node(v, g, new_l, 0))))
       else
         match g, d with
           Empty,_ -> d
          |_, Empty -> g
          |_ ->
            let min_val = (find_min d) in
            let new_r = (avl_supprimer d min_val) in
            (avl_reequilibre (avl_update_height (Node(min_val, g, new_r,0))))
;;


(*  2.2.1  *)

let rec create_aux t n : int avl_tree =
  if (n = 0 )
  then t
  else
    let random_value = Random.int(100) in (create_aux (avl_inserer t random_value) (n-1))
;;

let avl_rnd_create n : int avl_tree = (create_aux Empty n)
;;

let avl_linsert lis : 'a avl_tree =
  let abr : 'a avl_tree ref = ref (Empty) in
  let curList : 'a list ref = ref lis in
  while not(List.is_empty (!curList)) do(
    abr := (avl_inserer (!abr) (List.hd (!curList)));
    curList := List.tl (!curList);
    )done;
(!abr);;

let rec avg_rotations(t: 'a avl_tree): int =
  match t with
  | Empty -> 0
  | Node(_, g, d, rotations) ->
    let left_avg = avg_rotations(g) in
    let right_avg = avg_rotations (d) in
    (left_avg + right_avg + rotations)
;;


