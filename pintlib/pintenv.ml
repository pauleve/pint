
let _ =
    let pint_bin = Distenv.pint_share_path ^ "/bin"
    in
    Unix.putenv "PATH" (pint_bin^":"^Sys.getenv "PATH")

