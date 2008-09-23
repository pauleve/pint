
let spi = Spig.create 5 in
Spig.add spi "x0" (Spig.Take "x0y1", "x0");
Spig.add spi "x0" (Spig.Call "y0x0", "x1");
Spig.add spi "x1" (Spig.Take "x1y0", "x1");
Spig.add spi "x1" (Spig.Call "y1x1", "x0");
Spig.add spi "x1" (Spig.Call "y0x1", "x2");
Spig.add spi "x2" (Spig.Take "x2y0", "x2");
Spig.add spi "x2" (Spig.Delay "x2x2", "x2");
Spig.add spi "x2" (Spig.Call "y1x2", "x1");

Spig.add spi "y0" (Spig.Take "y0x0", "y0");
Spig.add spi "y0" (Spig.Take "y0x1", "y0");
Spig.add spi "y0" (Spig.Call "x1y0", "y1");
Spig.add spi "y0" (Spig.Call "x2y0", "y1");
Spig.add spi "y1" (Spig.Take "y1x1", "y1");
Spig.add spi "y1" (Spig.Take "y1x2", "y1");
Spig.add spi "y1" (Spig.Call "x0y1", "y0");


Io.dump_to_file "runs/pseudomonas.spig.dot" (Spig.to_dot spi);

let show_constraints cs =
	print_endline (Constraint.string_of_expression Spig.string_of_rname cs)
in

print_string "stable [x2;y1] => ";
let cs = Inference.exists spi (Inference.Stable ([], ["x2";"y1"]::[]))
in
show_constraints cs;

let cs = Inference.proportion spi 0.9 (Inference.Trace (["x0";"y0"]::["x1";"y0"]::["x1";"y1"]::["x0";"y1"]::["x0";"y0"]::[]))
in
show_constraints cs;


let states = List.flatten (List.map (fun ps1 -> List.map (fun ps2 -> [ps1;ps2]) ["y0";"y1"]) ["x0";"x1";"x2"])
in
let stateg = Graph.create (List.length states)
in
let push_state state = List.iter (fun edge -> Graph.add stateg state edge) (Spig.next spi state);
in
List.iter push_state states;
let data = Graph.to_dot stateg Spig.string_of_state Spig.string_of_transition
in
Io.dump_to_file "runs/pseudomonas.stateg.dot" data

