#load "clist.cmo";;
#load "ctree.cmo";;
#load "cnavl.cmo";;

open Ctree;;     
open Clist;;
open Cnavl;;

(* 1.2 *)

(* Expérimentation de déquilibre des ABR contruits à partir de suites de nombres entiers aléatoires *)
let experimentation_2(t) : unit  =
  let suite : 'a list ref = ref [] in
  let i : int ref = ref 0 in
  let count : int ref = ref 0 in
  
  print_endline("Tableau de déqulibres des arbres binaires de recherche contruits à partir de suites de nombres entiers aléatoires: ");
  print_newline();
  
  while (!i) < 1000  do(
    let bt : 'a t_btree = (bst_rnd_create t) in
  let deseq : int = (ab_desequilibre bt) in
  print_int(deseq);
  suite := (add_fst (!suite) deseq);
  i:=(!i)+1;
  count := (!count) + 1;

  if (!count) = 10 then (
      print_newline ();
      count := 0;
   )
) done;

let size : int = len(!suite) in
let total : int ref = ref 0 in
while not(isempty(!suite)) do(
  total := (!total)+(fst (!suite));
  suite := (rem_fst (!suite));
)done;
    print_newline();
    print_string("total: ");
    print_int(!total);
    print_newline();
    print_string("size: ");
    print_int(size);
    print_newline();
    print_string("resultat: ");
    print_int((!total)/size)
;;

(* 1.3 *)

(* Expérimentation de déqulibres des arbres binaires de recherche contruits à partir de suites ordonnés *)
let rec suite_ordonne(n, rnd : int * int) : 'a list =
  if n = 0
  then []
  else (add_lst (suite_ordonne(n-1, rnd)) (rnd+n))
;;

let generate_n_suite(n,t : int * int) : 'a list =
 let size : int ref = ref (n+1+Random.int(5)) in
 let count : int ref = ref n in
 let newList : 'a list ref = ref [] in
 while (!count) <> 0 do(
   let suite : 'a list = suite_ordonne(!size, Random.int(20)) in
   newList := (!newList)@suite;
   count := !count -1;
   if t == 0 then ()
   else
     if t == 1
        then size := Random.int(5)
              else if t == 2
                      then size := (!size)+1
                                   else size := (!size)-1
 )
done;
 !newList
;;

let experimentation_3(t : int) : unit  =
  let suite : 'a list ref = ref [] in
  let i : int ref = ref 0 in
  let count : int ref = ref 0 in
  
  print_endline("Tableau de déqulibres des arbres binaires de recherche contruits à partir de suites ordonnés: ");
  print_newline();
  
  while (!i) < 1000  do(
  let ls : int list = generate_n_suite(5, t) in
  let bt : 'a t_btree = (bst_lbuild ls) in
  let deseq : int = (ab_desequilibre bt) in
  print_int(deseq);
  print_char(' ');
  suite := (add_fst (!suite) deseq);
  i:=(!i)+1;
  count := (!count) + 1;

  if (!count) = 10 then (
      print_newline ();
      count := 0;
   )
  ) done;

  let size : int = (len (!suite)) in
  let total : int ref = ref 0 in
   while not(isempty (!suite)) do(
      total := (!total)+(fst (!suite));
      suite := (rem_fst (!suite));
    )done;
    print_newline();
    print_string("total: ");
    print_int(!total);
    print_newline();
    print_string("size: ");
    print_int(size);
    print_newline();
    print_string("resultat: ");
    print_int((!total)/size)
;;

let experimentation_RT(n: int) : unit =
  let suite : 'a list ref = ref [] in
  let i : int ref = ref 0 in
  let count : int ref = ref 0 in

  print_endline("Tableau de rotations: ");
  print_newline();
  
  while (!i) < 1000 do
    (
  let ls = generate_n_suite(5, n) in
  let bt : 'a avl_tree = avl_linsert(ls) in
  let rotations : int = avg_rotations (bt) in
  print_int(rotations);
  print_char(' ');
  suite := (add_fst(!suite) rotations);
  i:=(!i)+1;
  count := (!count) + 1;

  if (!count) = 10 then (
      print_newline ();
      count := 0;
   )
    ) done;
  
 let size : int = len(!suite) in
 let total : int ref = ref 0 in
 while not(isempty(!suite)) do
   (
   total := (!total)+fst(!suite);
   suite := rem_fst(!suite);
    )done;
    print_newline();
    print_string("total: ");
    print_int(!total);
    print_newline();
    print_string("size: ");
    print_int(size);
    print_newline();
    print_string("resultat: ");
    print_int((!total)/size)
;;
