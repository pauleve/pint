
let graph = Graph.create 2 in
Graph.add graph 1 ("1,+", 2);
Graph.add graph 1 ("2,+", 1);
Graph.add graph 2 ("1,-", 1);
print_string (Graph.to_dot graph string_of_int (fun x -> x))

