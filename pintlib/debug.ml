
let dodebug = ref true;;

let dbg_noendl msg = if !dodebug then (prerr_string msg; flush stderr) else ();;
let dbg msg = dbg_noendl (msg^"\n");;

