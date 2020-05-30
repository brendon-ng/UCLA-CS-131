(* Test Cases for Homework 1 *)

let my_subset_test0 = subset [] []
let my_subset_test1 = subset [3;5;6;7;7;7;7;7;7;4] [1;2;3;4;5;6;7;8;9;10]
let my_subset_test2 = not(subset [1;2;3] [])
let my_subset_test3 = subset [1;1;1;1;1;1] [1]
let my_subset_test4 = not(subset [3;4;5] [1;3;2])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;3] [1;1;3;3;3]
let my_equal_sets_test = not(equal_sets [1;2] [3;1;2])

let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [4;2;5] [4;2;6]) [4;2;6;5]

let my_set_intersection_test0 = equal_sets (set_intersection [] [1;2]) []
let my_set_intersection_test1 = equal_sets (set_intersection [1;2] []) []
let my_set_intersection_test2 = equal_sets (set_intersection [1;2] [3;2;1]) [1;2]

let my_set_diff_test0 = equal_sets (set_diff [1;5] [1;2;3;4;5]) []
let my_set_diff_test1 = equal_sets (set_diff [1;2;3;4;5] [1;2;3;4;5]) []
let my_set_diff_test2 = equal_sets (set_diff [1;2;3;4;5] [1;2]) [3;4;5]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x->x /. 3.) 10000000. = 0.
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x-> 3. *. x) 10. = infinity

type mathNT = 
| Expr | Op | Add | Sub | Num | Sym | Junk | Junk1 | Junk2

let mathRules =
[Expr, [N Expr; N Op; N Expr];
Expr, [N Expr; N Num];
Op, [N Add; N Sub];
Num, [T"0";T"1";T"2";T"3";T"4";T"5"];
Add, [N Sym];
Sub, [N Sym];
Sym, [T"+";T"-"];
Junk,[N Junk2; N Junk1];
Junk2, [N Junk1];
Junk1, [T"junk"]
]

let grammar = Expr, mathRules

let my_filter_reachable_test0 = not(filter_reachable grammar = grammar)
let my_filter_reachable_test1 = filter_reachable grammar = 
	(Expr, [Expr, [N Expr; N Op; N Expr];
		Expr, [N Expr; N Num];
Op, [N Add; N Sub];
Num, [T"0";T"1";T"2";T"3";T"4";T"5"];
Add, [N Sym];
Sub, [N Sym];
Sym, [T"+";T"-"]
]
	)

