type 'a avl_tree = Empty | Node of 'a * 'a avl_tree * 'a avl_tree * int
val avl_height : 'a avl_tree -> int
val avl_update_height : 'a avl_tree -> 'a avl_tree
val avl_desequilibre : 'a avl_tree -> int
val avl_rg : 'a avl_tree -> 'a avl_tree
val avl_rd : 'a avl_tree -> 'a avl_tree
val avl_rgd : 'a avl_tree -> 'a avl_tree
val avl_rdg : 'a avl_tree -> 'a avl_tree
val avl_reequilibre : 'a avl_tree -> 'a avl_tree
val avl_inserer : 'a avl_tree -> 'a -> 'a avl_tree
val find_min : 'a avl_tree -> 'a
val avl_supprimer : 'a avl_tree -> 'a -> 'a avl_tree
val create_aux : int avl_tree -> int -> int avl_tree
val avl_rnd_create : int -> int avl_tree
val avl_linsert : 'a list -> 'a avl_tree
val avg_rotations : 'a avl_tree -> int
