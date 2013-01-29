(*
Copyright or © or Copr. Loïc Paulevé, Morgan Magnin, Olivier Roux (2010)

loic.pauleve@irccyn.ec-nantes.fr
morgan.magnin@irccyn.ec-nantes.fr
olivier.roux@irccyn.ec-nantes.fr

This software is a computer program whose purpose is to provide Process
Hitting related tools.

This software is governed by the CeCILL license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the
CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author, the holder of the
economic rights, and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL license and that you accept its terms.
*)

open Debug;;

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


let rsa_of_firinginterval (d1,d2) cc =
	let round_sa saf = int_of_float (ceil saf)
	in
	let sa_hat u v (d1, d2) = exp (u *. ((d2/.d1)**v))
	and r_hat w x y (d1, d2) = (w +. x *. exp (-.y*.d1)) /. (d1+.d2)
	in
	let u, v, w, x, y =
		if cc = 0.99 then (6.41, -1.04, 2.03, 1.39, 33.33)
		else failwith ("Cannot compute firing interval at confidence coefficient "^string_of_float cc)
	in
	let r = r_hat w x y (d1, d2)
	and saf = sa_hat u v (d1, d2)
	in
	dbg ("["^string_of_float d1^";"^string_of_float d2^"] => "^string_of_float r^","^string_of_int (round_sa saf));
	r, round_sa saf
;;


