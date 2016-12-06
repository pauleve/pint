
import subprocess

from IPython.display import FileLink

import pint.config as cfg

def _run_tool(cmd, *args, input_model=None, **run_opts):

    if "stdout" not in run_opts:
        run_opts["stdout"] = subprocess.PIPE
    if "check" not in run_opts:
        run_opts["check"] = True

    if input_model is not None:
        if input_model.is_file():
            args += ["-i", input_model.src_file()]
        else:
            run_opts["input"] = input_model.src_data()
        if input_model.custom_initial_state():
            args += ["--initial-state", input_model.custom_inital_state()]

    return subprocess.run([cmd]+args, **run_opts)


format2ext = {
    "an": "an",
    "dump": "an",
    "nusmv": "smv",
    "pep": "ll",
    "romeo": "xml",
}

def export(model, format, *raw_args)
    assert format in format2ext
    assert "-o" not in args
    opath = cfg.new_output_filename(ext=format2ext[format])
    _run_tool("pint-export", "-l", format, "-o", opath, *raw_args,
                input_model=model, stdout=None)
    return FileLink(opath)

def cutsets(model, ai, maxsize=5, *args, exclude_initial_state=True, **opts):
    if exclude_initial_state:
        args.append("--no-init-cutsets")
    cp = _run_tool("pint-reach", "--cutsets", str(maxsize), *args,
                input_model=model, **opts)
    return cp.stdout.split()

