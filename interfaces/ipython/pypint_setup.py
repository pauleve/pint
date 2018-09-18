import subprocess
from colomoto.setup_helper import setup, PKG

__pint_required__ = "2018-07-10"
__PINT_VERSION__ = None

def check_pint_version():
    global __PINT_VERSION__
    version = subprocess.check_output(["pint-config", "version"]).decode()
    __PINT_VERSION__ = version.strip()
    return __pint_required__ <= __PINT_VERSION__

if __name__ == "__main__":
    setup({"pkg": "colomoto/pint", "check_progs": ["pint-config", "pint-reach"],
                "check_install": check_pint_version},
        PKG["clingo"],
        PKG["its"],
        PKG["nusmv"])
