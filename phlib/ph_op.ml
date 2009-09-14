
open Ph_types;;

let hitless_graph (ps, hits) =
	let has_hit ai bj =
		let hs = Hashtbl.find_all hits bj
		in
		let pred ((ai',p),j') = ai' = ai
		in
		List.exists pred hs
	in
	let folder v (a,la) = 
		let folder v i =
			let ai = (a,i)
			in
			if has_hit ai ai then v else (ai::v)
		in
		List.fold_left folder v (Util.range 0 la)
	in
	let v = List.fold_left folder [] ps
	in
	let v2 = Util.cross_list [v;v]
	in
	let folder set c =
		let (ai,bj) = List.nth c 0, List.nth c 1
		in
		if fst ai <> fst bj && not (has_hit ai bj || has_hit bj ai) then
			PCSet.add (ai,bj) set
		else
			set
	in
	v, List.fold_left folder PCSet.empty v2
;;

let ph_actions (ps,hits) =
	let folder bj ((ai,p),j') actions =
		Hit (ai,bj,j')::actions
	in
	Hashtbl.fold folder hits []
;;
let ph_count_actions (ps,hits) =
	Hashtbl.length hits
;;

let subph (ps,hits) sigma' =
	List.filter (fun (a,la) -> List.mem a sigma') ps,
	Hashtbl.copy hits
;;

let knockdown (ps,hits) a =
	let hits = Hashtbl.copy hits
	and la = List.assoc a ps
	in
	let rec remove_all i =
		if Hashtbl.mem hits (a,i) then (
			Hashtbl.remove hits (a,i);
			remove_all i
		)
	in
	List.iter remove_all (Util.range 0 la);
	(a,0)::List.remove_assoc a ps, hits
;;


