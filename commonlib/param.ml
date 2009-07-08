
let firing_interval alpha r sa =
	let rate = r*.float_of_int sa
	and alpha = alpha/.2.
	in
	R.qerlang alpha sa rate true, R.qerlang alpha sa rate false
;;


