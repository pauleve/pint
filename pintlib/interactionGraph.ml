(***
Copyright Loïc Paulevé <loic.pauleve@ens-cachan.org> (2014)

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
**)

open PintTypes;;

type sign_t = Positive | Negative
type regulation_t = Regulation of (string * int * sign_t * string * stochatime)

let string_of_sign = function Positive -> "+" | Negative -> "-"

module IG =
struct
	type res_t = SSet.t
	type t = ((string * int * sign_t) list) SMap.t

	let to_string ig =
		let string_of_regulation a regs buf =
			buf ^ (String.concat ";\n" (List.map
				(fun (b, i, s) ->
					b ^ " " ^ string_of_int i ^ " -> "
						^ string_of_sign s ^ " " ^ a) regs))
			^ ";\n"
		in
		SMap.fold string_of_regulation ig ""

	let regulators ig a =
		let fold_regulation bs (b, _, _) =
			SSet.add b bs
		in
		List.fold_left fold_regulation SSet.empty (SMap.find a ig)

	let ctx_for_resources ig ps a res =
		let fold_regulation ctx (b, th, s) =
			let l = List.assoc b ps
			in
			match (s, SSet.mem b res) with
			  (Positive, true) | (Negative, false) ->
					SMap.add b (iset_of_list (Util.range th l)) ctx
			| (Positive, false) | (Negative, true) ->
					SMap.add b (iset_of_list (Util.range 0 (th-1))) ctx
		in
		List.fold_left fold_regulation SMap.empty (SMap.find a ig)

	let extended_local_ctx ?safe:(safe=false) ig ps a gctx =
		let fold_regulation ctx (b, th, _) =
			let l = List.assoc b ps
			and js = SMap.find b gctx
			in
			let above_th = ISet.for_all (fun j -> j >= th) js
			in
			(if safe then
				assert (above_th || ISet.for_all (fun j -> j < th) js));
			if above_th then
				SMap.add b (iset_of_list (Util.range th l)) ctx
			else
				SMap.add b (iset_of_list (Util.range 0 (th-1))) ctx
		in
		List.fold_left fold_regulation SMap.empty (SMap.find a ig)

end


