val len : 'a list -> int
val empty : 'a list
val isempty : 'a list -> bool
val fst : 'a list -> 'a
val lst : 'a list -> 'a
val nth : 'a list -> int -> 'a
val add_fst : 'a list -> 'a -> 'a list
val add_lst : 'a list -> 'a -> 'a list
val add_nth : 'a list -> 'a -> int -> 'a list
val rem_fst : 'a list -> 'a list
val rem_lst : 'a list -> 'a list
val rem_nth : 'a list -> int -> 'a list
val concat : 'a list -> 'a list -> 'a list
val list_rnd_create : int -> int list
