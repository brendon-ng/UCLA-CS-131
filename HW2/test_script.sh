echo "Performing convert_grammar test:"
printf "#use \"hw2.ml\";;\n#use \"convert_test.ml\";;" | ocaml
printf "Completed convert_grammar test\n"
echo "Performing parse tree test"
#printf "#use \"hw2.ml\"\nlet test5 = (parse_tree_leaves (Node (\"+\", [Leaf 3; Node (\"*\", [Leaf 4; Leaf 5])])) = [3; 4; 5]);;" | ocaml
#printf "Completed parse tree test\n"
echo "Performing sample test"
printf "#use \"hw2.ml\";;\n#use \"hw2sample.ml\";;" | ocaml
echo "Completed sample test"
