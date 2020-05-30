(* HW 1 CS 131 *)

type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal

(* 1 *)
let subset a b =
List.for_all (fun x -> List.exists (fun y -> x=y) b) a;;

(* 2 *)
let equal_sets a b =
subset a b && subset b a;;

(* 3 *)
let rec set_union a b =
match a with
| head::tail -> head::(set_union tail b)
| _ -> b;; 

(* 4 *)
let set_intersection a b =
List.filter (fun x -> List.exists (fun y -> y=x) b) a;;

(* 5 *)
let set_diff a b =
List.filter( fun x -> not (List.exists(fun y -> y=x) b) ) a;;

(* 6 *)
let rec computed_fixed_point eq f x =
if (eq) (f x) x then x else (computed_fixed_point eq f (f x));;

(* 7 *)

let equal_second_elem_sets a b = equal_sets (snd a) (snd b);;
let is_element a b = List.exists (fun x-> x=a) b;;
let isNT a = match a with
| N nonterminal -> true
| T terminal -> false;;


let rec filterNT lst = match lst with
| N head::tail -> head::(filterNT tail)
| T head::tail -> filterNT tail
|[] -> [];;

let rec get_current_reachable rules current = match rules with
| head::tail -> let lhs = (fst head) in if (is_element lhs current) then
                                         let rhs = filterNT (snd head) in let next = set_union current rhs in get_current_reachable tail next
                                        else get_current_reachable tail current
| [] -> current;;

let rec get_all_reachable rules current =
let next = get_current_reachable rules current in
if equal_sets current next then current else get_all_reachable rules next;;

let filter_reachable g =
let start_symbol = (fst g) in
let rules = (snd g) in
let reachable = get_all_reachable rules [start_symbol] in
(start_symbol, List.filter (fun x-> is_element (fst x) reachable) rules);;
