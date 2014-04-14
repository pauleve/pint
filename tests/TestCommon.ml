
let rec cs_next_word cs =
	try
		match Stream.next cs with
		  '\n' | ' ' -> ""
		| c -> Char.escaped c ^ cs_next_word cs
	with Stream.Failure -> ""


