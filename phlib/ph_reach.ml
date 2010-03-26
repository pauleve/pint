
open Debug;;

type bounce_path = sort * sortidx * ISet.t
type bounce_sequence = action list

module BounceSequenceOT = struct type t = bounce_sequence let compare = compare end
module BS = Set.Make (BounceSequenceOT)

let string_of_bounce_path (a, i, js) =
	string_of_process (a,i) ^ " " ^ string_of_iset js
;;
let string_of_bounce_sequence bs =
	"["^(String.concat "; " (List.map string_of_action bs))^"]"
;;
let string_of_BS = string_of_set string_of_bounce_sequence;;

type env = {
	sorts : process list;
	t_hits : hits;
	_BS : (bounce_path, BS.t) Hashtbl.t;
}

let compute_BS env bp =
	let a, i, reachset = bp
	in
	let rec walk j visited =
		if ISet.mem j reachset then
			BS.singleton []
		else
			let visited = ISet.add j visited
			and actions = Hashtbl.find_all env.t_hits (a,j)
			in
			let folder results = function (hitter,_),k ->
				if ISet.mem k visited then
					results
				else
					let k_results = walk k visited
					in
					push_results (Hit (hitter,(a,j),k)) k_results results
			in
			List.fold_left folder BS.empty actions
	in
	(*DEBUG*) dbg_noendl ("- computing BS("^string_of_bounce_path bp^")..."); (**)
	let result = walk i ISet.empty
	in
	(*DEBUG*) dbg (" "^string_of_BS result); (**)
	Hashtbl.add env._BS bp result;
	result
;;


let create_env (ps,hits) = 
	{
		sorts = ps;
		t_hits = hits;
		_BS = Hashtbl.create 50;
	}
;;

let _BS env bp =
	try Hashtbl.find env._BS bp
	with Not_found -> compute_BS env bp
;;


