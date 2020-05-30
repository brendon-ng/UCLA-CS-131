(* HW 2 CS 131 *)

type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(* 1 *)
let rec f hw1rules x = match hw1rules with
| head::tail -> if (fst head) = x then (snd head)::(f tail x) else (f tail x)
| [] -> [];;

let convert_grammar gram1 = (fst gram1, f (snd gram1));;


(* 2 *)
let rec parse_tree_leaves = function
| Leaf l -> [l]
| Node (a,b) -> parse_children b
and parse_children = function
| [] -> []
| (Node (a,b))::tail -> (parse_tree_leaves (Node (a,b))) @ (parse_children tail)
| (Leaf l)::tail -> l::(parse_children tail);;

(* 3 *)

let rec matcher prod_func rules accept frag =
(* Iterate all rules for start symbol *) 
match rules with
| (cur_rule::rulestail) -> (* Try the rule to see if it matches/accepts *)
                          (match (try_rule prod_func cur_rule accept frag) with
                          | Some x -> Some x (* Rule/suffix accepted *)
                          | None -> matcher prod_func rulestail accept frag )
                                    (* Next Rule, loop recursive iterator*)
| [] -> None (* Doesn't match any rule *)

and try_rule prod_func cur_rule accept frag = 
(* Iterate symbols left to right *)
match cur_rule with
| (symbol::symbol_tail) -> (* NT or terminal? essentially an if else and a loop*)
                          (match symbol with
                          | N nt -> let new_accept = try_rule prod_func symbol_tail accept in matcher prod_func (prod_func nt) new_accept frag 
                          | T terminal -> (match frag with
                                          | fhead::ftail -> if fhead=terminal then try_rule prod_func symbol_tail accept ftail (* check rest of frag *) else None 
                                          | [] -> None )) (* rule has too many symbols *)
| [] -> accept frag;; (* all symbols match! Try acceptor*)

let make_matcher gram =
let prod_func = (snd gram) in
(fun accept frag -> matcher prod_func (prod_func (fst gram)) accept frag);;

(* 4 *)

let rec parser prod_func rules accept frag =
(* Iterate all rules for start symbol *) 
match rules with
| (cur_rule::rulestail) -> (* Try the rule to see if it matches/accepts *)
                          (match (parse_tree prod_func cur_rule accept frag) with
                          | Some x -> Some (cur_rule::x) (* Rule/suffix accepted *)
                          | None -> parser prod_func rulestail accept frag )
                                    (* Next Rule, loop recursive iterator*)
| [] -> None (* Doesn't match any rule *)

and parse_tree prod_func cur_rule accept frag = 
(* Iterate symbols left to right *)
match cur_rule with
| (symbol::symbol_tail) -> (* NT or terminal? essentially an if else *)
                          (match symbol with
                          | N nt -> let new_accept = parse_tree prod_func symbol_tail accept in 
                                    parser prod_func (prod_func nt) new_accept frag
                          | T terminal -> (match frag with
                                          | fhead::ftail -> if fhead=terminal then parse_tree prod_func symbol_tail accept ftail (* check rest of frag *) else None 
                                          | [] -> None )) (* rule has too many symbols *)
| [] -> accept frag;; (* all symbols match! Try acceptor*)

let rec horiz_parse this_level rules =
match this_level with
| head::tail -> (match vert_parse head rules with
                 | (a, b) -> (match horiz_parse tail a with
				    | (c, d) -> (c, b::d)
                             )
                )
| [] -> (rules, [])

and vert_parse root rules =
match root with
| T term -> (match rules with
            | [] -> ([], Leaf term)
	    | _ -> (rules, Leaf term)
            )
| N nt ->   (match rules with
	    | [] -> ([], Node (nt, []))
            | rules_head::rules_tail -> (match horiz_parse rules_head rules_tail with
					 | (a, b) -> (a, Node (nt, b))
                                        )
            );;

let empty_suffix = function
| [] -> Some []
| _ -> None;;


let make_parser gram =
let prod_func = (snd gram) in
fun frag -> match (parser prod_func (prod_func (fst gram)) empty_suffix frag) with
| Some reached_rules -> (match horiz_parse [N (fst gram)] reached_rules with
            | (_,head::tail) -> Some head
            | _ -> None)
| None -> None;;

