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

(* Thomas' logicial parameters: gene * activators * value *)
type parameter_thomas = Decision.metaproc * Decision.metaproc list * int
;;

let decisions_of_parameter_thomas grn (a, activators, value) =
	let folder b ((t,e),a') infos =
		if a = a' then (
			let levels = (if List.mem b activators && e = Brg.Activation
				|| not (List.mem b activators) && e = Brg.Inhibition
			then Util.range t (Brg.level_max grn b) else (Util.range 0 (t-1)))
			in
			Util.cross infos (List.map (fun l -> b, l) levels)
		) else infos
	in
	let infos = Brg.fold folder grn [[]]
	in
	assert (List.length infos > 0);
	let infos = if not (List.mem a (fst (List.split (List.hd infos)))) then
					let levels = Util.range 0 (Brg.level_max grn a)
					in
					Util.cross infos (List.map (fun l -> a, l) levels)
				else infos
	in
	let make_decision state =
		let _,x = Decision.get_proc_by_meta a state
		and sgn diff = if diff < 0 then Decision.Dec
						else if diff > 0 then Decision.Inc
						else Decision.Dis
		and info_from_p p =
			Decision.L (fst p, string_of_int (snd p))
		in
		a, (List.map info_from_p state), sgn (value - x)
	in
	let decisions = List.map make_decision infos
	in
	List.filter (fun (m,i,a) -> a != Decision.Dis) decisions
;;


