(*
 * Name:Jonathan Rudnick
 * Assignment: Programming Languages Homework 4
 *
 *)


(* Problem 1: mapFunToList function definition *)
datatype BST = Empty
| Node of BST * int * BST;

fun insert item Empty = Node (Empty, item, Empty)
| insert item (Node (left, v, right)) =
      if item < v then (Node (insert item left, v, right))
      else if item > v then (Node (left, v, insert item right))
      else (Node (left, v, right));

fun search item Empty = false
| search item (Node (left, v, right)) = if item = v then true
    else if item > v then search item right else search item left;

fun inOrderList Empty = []
| inOrderList (Node (left, v, right)) = inOrderList left @ [v] @ inOrderList right;

(* Problem 1 testing *)
val t = insert 50 Empty;
val t = insert 100 t;
val t = insert 20 t;
val t = insert 40 t;
inOrderList t;