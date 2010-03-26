
let dbg_noendl msg = output_string stderr msg; flush stderr;;
let dbg msg = dbg_noendl (msg^"\n");;

