
let dbg_noendl msg = prerr_string msg; flush stderr;;
let dbg msg = dbg_noendl (msg^"\n");;

