type spanish_nonterminals =
  | Sentence | Object | Noun | Action | Verb | Adjective | Adverb | The   

let prod_func = function
	| Sentence -> 
		[[N Object; N Action; N Object];
                 [N Object; N Action]; 
                 [N Object; N Verb];
                 [N Sentence; T ","; N Sentence]
                 ]
	| Object ->
		[[N The; N Object];
		 [N Noun; N Adjective];
                 [N Noun]]
	| Noun ->
		[[T "perro"];
		 [T "Senor"; T "Eggert"]]
	| Action ->
		[[N Verb; N Adverb];
		 [N Verb]]
	| Verb ->
		[[T "ensena"];
		 [T "corre"];
		 [T "duerma"]]
        | Adjective ->
                 [[T "feliz"];
                  [T "pequeno"]]
        | Adverb ->
                 [[T "lentamente"];
                  [T "rapidamente"]]
        | The -> [[T "el"]; [T "la"]; [T "un"]];;

let spanish = (Sentence, prod_func);;

let accept_punctuation_suffix = function
	| [] -> None
	| x -> Some x


let frag = ["Senor"; "Eggert"; "ensena"; ","; "el"; "perro"; "pequeno"; "duerma"; "lentamente"]

let make_matcher_test = 
	((make_matcher spanish accept_punctuation_suffix frag)
		= Some [","; "el"; "perro"; "pequeno"; "duerma"; "lentamente"])

let make_parser_test = 
	match make_parser spanish frag with
	| Some tree -> parse_tree_leaves tree = frag
	| _ -> false

