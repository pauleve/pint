
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

let ph_index index actions =
	let hactions = Hashtbl.create 0
	in
	let register action = 
		Hashtbl.add hactions (index action) action
	in
	List.iter register actions;
	hactions
;;
		

let subph (ps,hits) sigma' =
	List.filter (fun (a,la) -> List.mem a sigma') ps,
	Hashtbl.copy hits
;;

let ph_from_actions actions =
	let hits = Hashtbl.create 0
	in
	let update_ps ps (a,i) =
		try
			let i' = List.assoc a ps
			in
			if i > i' then (a,i)::List.remove_assoc a ps else ps
		with Not_found -> (a,i)::ps
	in
	let folder ps = function Hit(ai,bj,k) ->
		Hashtbl.add hits bj ((ai,None),k);
		update_ps (update_ps (update_ps ps ai) bj) (fst bj,k)
	in
	let ps = List.fold_left folder [] actions
	in
	ps,hits
;;


