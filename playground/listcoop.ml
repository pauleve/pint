

open Ph_types;;

let show_cooperativity automaton _ = print_endline automaton
in
let _ = Ui.simple_input ()
in
SMap.iter show_cooperativity !Ph_instance.cooperativities


