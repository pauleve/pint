
let firing_interval alpha r sa =
	let rate = r*.float_of_int sa
	and alpha = alpha/.2.
	in
	R.qerlang alpha sa rate true, R.qerlang alpha sa rate false
;;

let random_firing_time r sa =
	R.rerlang sa (r*.float_of_int sa)
;;

let round_float v = (* assume v >= 0 *)
	let f = floor v
	in
	if v -. f > 0.5 then ceil v else f
;;

let int_of_fi fi = int_of_float (fst fi), int_of_float (snd fi);;
let round_fi_closest fi = int_of_fi (round_float (fst fi), round_float (snd fi));;
let round_fi_ex fi = int_of_fi (floor (fst fi), ceil (snd fi));;
let round_fi_in fi = int_of_fi (ceil (fst fi), floor (snd fi));;
let zoom_fi factor fi = factor*.fst fi, factor*.snd fi;;

