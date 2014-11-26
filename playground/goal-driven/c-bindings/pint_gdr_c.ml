
(* pintc -output-obj -o pint_gdr_caml.o pint_gdr_c.ml *)

let ph_size ph = Ph_util.count_actions ph

let ph_parse filename =
	Ph_util.parse (open_in filename)

let _ = Callback.register "ph_parse" ph_parse
let _ = Callback.register "ph_size" ph_size

