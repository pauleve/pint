open Debug;;
open Ph_types;;
open Ph_abstr_struct;;

let pl = [("a", 1)]
in
let ph, state = Ph_util.parse (open_in "test_mincont.ph")
in
let w = Ph_reach.objseq_from_procseq state pl
in
let ctx = ctx_of_state state
in
let bs_cache = Ph_bounce_seq.new_bs_cache ();
in
let gA = new cwA ctx w (Ph_bounce_seq.get_aBS ph bs_cache)
in
List.iter (fun obj -> gA#init_proc (obj_bounce_proc obj)) w;
gA#commit ();
gA#debug ();

let d_min_procs = Hashtbl.create 50
in
min_procs gA d_min_procs (ObjSet.elements gA#objs);

let debug_min_procs obj =
	let ctx, _ = Hashtbl.find d_min_procs (NodeObj obj)
	in
	dbg ("minProc("^string_of_obj obj^") = "^string_of_ctx ctx);
in
List.iter debug_min_procs (ObjSet.elements gA#objs)


