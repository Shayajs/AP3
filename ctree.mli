type 'a t_btree = Vide | B_RT of ('a * 'a t_btree * 'a t_btree)
val bt_empty : 'a t_btree
val bt_rooting : 'a -> 'a t_btree -> 'a t_btree -> 'a t_btree
val bt_isempty : 'a t_btree -> bool
val bt_root : 'a t_btree -> 'a
val bt_subleft : 'a t_btree -> 'a t_btree
val bt_subright : 'a t_btree -> 'a t_btree
val bt_size : 'a t_btree -> int
val bt_height : 'a t_btree -> int
val bst_seek : 'a t_btree -> 'a -> bool
val bst_insert : 'a t_btree -> 'a -> 'a t_btree
val delete_min : 'a t_btree -> 'a t_btree * 'a
val bst_delete : 'a t_btree -> 'a -> 'a t_btree
val ab_desequilibre : 'a t_btree -> int
val bst_lbuild : 'a list -> 'a t_btree
val bst_cut : 'a t_btree -> 'a -> 'a t_btree * 'a t_btree
val list_rnd_create : int -> int list
val bst_rnd_create : int -> int t_btree
val print_val : int -> int -> unit
val see_btree : int t_btree -> int -> unit
val bst_isBstRed : 'a t_btree -> 'a -> bool -> bool
val bst_isBst : 'a t_btree -> bool
val ab_rg : 'a t_btree -> 'a t_btree
val ab_rd : 'a t_btree -> 'a t_btree
val ab_rgd : 'a t_btree -> 'a t_btree
val ab_rdg : 'a t_btree -> 'a t_btree
