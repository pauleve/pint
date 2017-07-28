
open PintTypes
open AutomataNetwork

let register_and_collect_changes an ?(changes=ObjSet.empty) trid tr =
    let fold a i changes =
        let j = IMap.find a tr.dest
        in
        Hashtbl.add an.change2tr ((a,i),j) trid;
        ObjSet.add (a,i,j) changes
    in
    IMap.fold fold tr.orig changes

let _pull_changes an =
    ObjSet.iter (fun (a,i,j) -> Hashtbl.add an.lsnext (a,i) j)

let _custom_copy_transitions an an' register_transition =
    let changes = Hashtbl.fold register_transition an.trs ObjSet.empty
    in
    _pull_changes an' changes

let _hashtbl_filtered_copy filter h h' =
    Hashtbl.iter (fun k v -> if filter k v then
        Hashtbl.add h' k v) h

(** Creates a new automata network which consits of only the automata aset in an
    and where conditions outside of aset from transitions are removed *)
let partial an aset =
    let an' = new_an ~nb_a:(ISet.cardinal aset) ()
    in
    _hashtbl_filtered_copy (fun a _ -> ISet.mem a aset)
        an.ls an'.ls;
    _hashtbl_filtered_copy (fun a _ -> ISet.mem a aset)
        an.a2sig an'.a2sig;
    _hashtbl_filtered_copy (fun _ a -> ISet.mem a aset)
        an.sig2a an'.sig2a;
    _hashtbl_filtered_copy (fun (a,_) _ -> ISet.mem a aset)
        an.ls2sig an'.ls2sig;
    _hashtbl_filtered_copy (fun _ (a,_) -> ISet.mem a aset)
        an.sig2ls an'.sig2ls;
    let filter_state =
        IMap.filter (fun a _ -> ISet.mem a aset)
    in
    let filter_tr tr = {
            orig = filter_state tr.orig;
            dest = filter_state tr.dest;
            cond = filter_state tr.cond;
            pre = filter_state tr.pre
        }
    in
    let register_transition trid tr changes =
        let tr = filter_tr tr
        in
        if IMap.is_empty tr.orig then changes
        else (
            Hashtbl.add an'.trs trid tr;
            register_and_collect_changes an' trid tr ~changes
        )
    in
    _custom_copy_transitions an an' register_transition;
	an'


let remove_sink_automaton an a =
    let sig_a = label_of_a an a
    and n = automaton_length an a
    in
    let is = Util.range 0 (n-1)
    in
    Hashtbl.remove an.a2sig a;
    Hashtbl.remove an.sig2a sig_a;
    Hashtbl.remove an.ls a;
    List.iter (fun i ->
        let ls = (a,i)
        in
        let sig_ls = Hashtbl.find an.ls2sig ls
        in
        Hashtbl.remove an.ls2sig ls;
        Hashtbl.remove an.sig2ls sig_ls;
        let js = Hashtbl.find_all an.lsnext ls
        in
        List.iter (fun j ->
            let trs = Hashtbl.find_all an.change2tr (ls,j)
            in
            List.iter (Hashtbl.remove an.trs) trs;
            List.iter (fun _ -> Hashtbl.remove an.change2tr (ls,j)) trs) js;
        List.iter (fun _ -> Hashtbl.remove an.lsnext ls) js) is


let imap_of_bindings = map_of_bindings IMap.add IMap.empty

let an_with_filtered_transitions ?(nb_trs=0) an filter =
    let nb_trs = if nb_trs == 0 then count_transitions an / 2 else nb_trs
    in
    let an' = new_an_copy_def ~nb_trs an
    in
    let pull_tr trid tr changes = if filter trid tr then
        let _ = Hashtbl.add an'.trs trid tr
        in
        register_and_collect_changes an' trid tr ~changes else changes
    in
    let changes = Hashtbl.fold pull_tr an.trs ObjSet.empty
    in
    _pull_changes an' changes;
    an'

let simplify an =
	let sd = Hashtbl.fold (fun a n -> IMap.add a (Util.range 0 (n-1)))
                an.ls IMap.empty
    and groups = Hashtbl.create (Hashtbl.length an.change2tr)
    in
    let group_tr trid tr keys =
        let key = IMap.bindings tr.orig, IMap.bindings tr.dest
        in
        let keys = if not (Hashtbl.mem groups key) then
            key::keys else keys
        in
        Hashtbl.add groups key tr.cond;
        keys
    in
    let keys = Hashtbl.fold group_tr an.trs [];
    in
    let an' = new_an_copy_def ~nb_trs:(count_transitions an / 2) an
    in
    let push_tr orig dest changes cond =
        let trid = new_id ()
        and tr = new_transition orig dest cond
        in
        Hashtbl.add an'.trs trid tr;
        register_and_collect_changes an' trid tr ~changes
    in
	let simplify_group changes key =
        let vs = Hashtbl.find_all groups key
        in
        let vs = List.sort_uniq (IMap.compare compare) vs
        in
		let vs = ValSet.simplify_with_bse sd vs
		in
        let orig = imap_of_bindings (fst key)
        and dest = imap_of_bindings (snd key)
        in
		List.fold_left (push_tr orig dest) changes vs
	in
    let changes = List.fold_left simplify_group ObjSet.empty keys
    in
    _pull_changes an' changes;
    an'


(** [squeeze an ctx] removes constant automata and unused local states. *)
let squeeze ?(preserve=ISet.empty) an ctx =
    let dynconditions a _ imap =
        let is = IMap.find a ctx
        in
        if not (ISet.mem a preserve) && ISet.cardinal is == 1 then
            let i = ISet.choose is
            in
            let js = Hashtbl.find_all an.lsnext (a,i)
            in
            let trids = List.fold_left (fun trids j ->
                let trs = Hashtbl.find_all an.change2tr ((a,i),j)
                in
                List.fold_left (fun trids trid -> ISet.add trid trids) trids trs)
                    ISet.empty js
            in
            IMap.add a trids imap
        else imap
    and constant_map csts =
        ISet.fold (fun a -> IMap.add a (ISet.choose (IMap.find a ctx))) csts IMap.empty
    and dead_transitions cmap trid tr deads =
        if IMap.exists (fun a i -> IMap.mem a tr.pre && IMap.find a tr.pre != i) cmap
        then ISet.add trid deads else deads
    in
    let rec propagate_constants cmap dyntrs =
        let deads = Hashtbl.fold (dead_transitions cmap) an.trs ISet.empty
        in
        let dyntrs = IMap.map (fun trids -> ISet.diff trids deads) dyntrs
        in
        let new_csts = IMap.fold (fun a trids csts ->
            if ISet.is_empty trids then ISet.add a csts else csts) dyntrs ISet.empty
        in
        if ISet.is_empty new_csts then (cmap, deads)
        else
            let cmap' = constant_map new_csts
            and dyntrs = IMap.filter (fun a trids -> not (ISet.is_empty trids)) dyntrs
            in
            let cmap', deads' = propagate_constants cmap' dyntrs
            in
            IMap.union (fun _ v _ -> Some v) cmap cmap',
            ISet.union deads deads'
    in
	(* get constant automata *)
	let csts = constants an ctx
    in
    let csts = ISet.diff csts preserve
	in
    let dyntrs = Hashtbl.fold dynconditions an.ls IMap.empty
    and cmap = constant_map csts
    in
    let cmap, deads = propagate_constants cmap dyntrs
    in
    let keep_tr trid = not (ISet.mem trid deads)
    in

    (* collect used local states *)
    let collect_ls a i = ctx_add_ls (a,i)
    in
    let col = Hashtbl.fold (fun trid tr col -> if keep_tr trid then
        let col = IMap.fold collect_ls tr.pre col
        in
        IMap.fold collect_ls tr.dest col else col) an.trs IMap.empty
    in
    let col = IMap.filter (fun a is ->
        not (IMap.mem a cmap || ISet.mem a preserve)
            && automaton_length an a > ISet.cardinal is) col
    in
    let ls_rename_map is =
        let phi = List.mapi (fun i' i -> (i,i')) (ISet.elements is)
        in
        List.fold_left (fun imap (i,i') -> IMap.add i i' imap) IMap.empty phi
    in
    let rename_map = IMap.map ls_rename_map col
    in
    let project_a rename a v imap =
        if IMap.mem a cmap then imap else
        let v = match IMap.find_opt a rename_map with
              Some r -> rename r v
            | None -> v
        in
        IMap.add a v imap
    in
    let project_state state =
        let project = project_a (fun r i -> IMap.find i r)
        in
        IMap.fold project state IMap.empty
    and project_ctx ctx =
        let project = project_a (fun r -> ISet.map (fun i -> IMap.find i r))
        in
        IMap.fold project ctx IMap.empty
    in

    let nb_a = count_automata an - IMap.cardinal cmap
    and nb_trs = count_transitions an - ISet.cardinal deads
    in
    let an' = new_an ~nb_a ~nb_trs ()
    in
    let keep_a a = not (IMap.mem a cmap)
    and rename a i =
        match IMap.find_opt a rename_map with
          Some r -> IMap.find_opt i r
        | None -> Some i
    in

    Hashtbl.iter (fun a n -> if keep_a a then
            let n = match IMap.find_opt a rename_map with
                  Some r -> IMap.cardinal r
                | None -> n
            in
            Hashtbl.add an'.ls a n) an.ls;
    _hashtbl_filtered_copy (fun a _ -> keep_a a) an.a2sig an'.a2sig;
    _hashtbl_filtered_copy (fun _ a -> keep_a a) an.sig2a an'.sig2a;
    Hashtbl.iter (fun (a,i) sig_ls -> if keep_a a then
        match rename a i with None -> ()
        |  Some i -> Hashtbl.add an'.ls2sig (a,i) sig_ls) an.ls2sig;
    Hashtbl.iter (fun sig_ls (a,i) -> if keep_a a then
        match rename a i with None -> ()
        |  Some i -> Hashtbl.add an'.sig2ls sig_ls (a,i)) an.sig2ls;

    let project_transition tr = {
            orig = project_state tr.orig;
            dest = project_state tr.dest;
            cond = project_state tr.cond;
            pre = project_state tr.pre
        }
    in
    Hashtbl.iter (fun trid tr -> if keep_tr trid then
            Hashtbl.add an'.trs trid (project_transition tr)) an.trs;
    let changes = Hashtbl.fold (fun ((a,i),j) trid changes -> if keep_tr trid then
            let i = match rename a i with Some i -> i | None -> failwith "oops (1)"
            and j = match rename a j with Some i -> i | None -> 
                failwith ("oops (2) "^string_of_int a^" "^string_of_int j)
            in
            let _ = Hashtbl.add an'.change2tr ((a,i),j) trid
            in
            ObjSet.add (a,i,j) changes
        else changes) an.change2tr ObjSet.empty
    in
    _pull_changes an' changes;
    an', project_ctx ctx

