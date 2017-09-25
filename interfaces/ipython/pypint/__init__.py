"""
When loaded, the `pypint` module will perform the follow tasks:

* add Pint third-party binary path (`bin` subdirectory of
  ``pint-config share-path``) to the ``PATH`` environment variable.
* if in IPython, displays the version of Pint binaries, and executes
  :py:func:`.ipython_install`.
"""
__version__ = "1.3.1"
__pint_required__ = "2017-07-28"

import os
import shutil
import subprocess
import sys

if sys.version_info[0] < 3:
    raise Exception("python >= 3 is required")

from .cfg import *
from .types import *
from .model import *
from .ui import *
from .tools import *
from .utils import *

__SETUP_DONE = False

__PINT_VERSION__ = None

def check_pint():
    if shutil.which("pint-config") is None:
        raise Exception("Pint binaries should be installed separately. " + \
            "See https://loicpauleve.name/pint/doc/#Binaries")

    global __PINT_VERSION__
    version = subprocess.check_output(["pint-config", "version"]).decode()
    __PINT_VERSION__ = version.strip()

    if __pint_required__ > __PINT_VERSION__:
        raise Exception("Pint >= {0} is required. Please upgrade Pint binaries. " \
                .format(__pint_required__) +  \
            "See https://loicpauleve.name/pint/doc/#Binaries" )


def setup_environ():
    check_pint()
    global __SETUP_DONE
    share_path = subprocess.check_output(["pint-config", "share-path"]).decode()
    bin_path = os.path.join(share_path.strip(), "bin")
    os.environ["PATH"] = "%s:%s" % (bin_path, os.environ["PATH"])
    __SETUP_DONE = True

def hello_ipython():
    info("You are using Pint version %s and pypint %s" % (__PINT_VERSION__,__version__))
    info("API is documented at https://loicpauleve.name/pint/doc/api.html")


if not __SETUP_DONE:
    setup_environ()

if IN_IPYTHON:
    hello_ipython()
    from colomoto_jupyter import jupyter_setup

    menu = [
        {"name":"Load model",
            "snippet":["model = pypint.load(\"filename_or_URL\")"]},
        {"name":"Upload model",
            "snippet":["model = pypint.load()"]},
        "---",
        {"name":"Model description",
            "sub-menu": [
            {"name": "Dependency graph",
                "snippet":['model.dependency_graph()']},
            {"name": "Model summary",
                "snippet":['model.summary()']}
            ]},
        {"name":"Model export",
            "sub-menu": [
            {"name": "Pint native format (.an)",
                "snippet":['model.export("an")']},
            {"name": "NuSMV model (.smv)",
                "snippet":['model.export("nusmv")']},
            {"name": "Safe Petri net in PEP format (.ll)",
                "snippet":['model.export("pep")']},
            {"name": "Safe Petri net in ROMEO format (.xml)",
                "snippet":['model.export("romeo")']},
            ]},
        "---",
        {"name":"Model transformation",
            "sub-menu": [
            {"name": "Change initial state",
                "snippet":['model.having(a=1,b=1)']},
            {"name": "Lock automata (mutations)",
                "snippet":['model.lock({"a":1,"b":0})']},
            {"name": "Disable local states",
                "snippet":['model.disable({"a":1,"b":1})']},
            {"name": "Goal-oriented reduction",
                "snippet":['model.reduce_for_goal("a=1")']}
            ]},
        "---",
        {"name":"Compute mutations for cutting goal reachability",
            "snippet":['model.oneshot_mutations_for_cut("a=1")']},
        {"name":"Compute cut sets of paths to goal",
            "snippet":['model.cutsets("a=1")']},
        {"name":"Compute bifurcation transitions from goal",
            "snippet":['model.bifurcations("a=1")']},
        {"name":"Verify reachability of goal",
            "snippet":['model.reachability("a=1")']},
        "---",
        {"name":"Local Causality Graph",
            "sub-menu": [
            {"name": "Full LCG", "snippet":['model.full_lcg()']},
            {"name": "Simple LCG for goal reachability over-approximation",
                "snippet":['model.simple_lcg("a=1")']},
            {"name": "Saturated LCG for goal reachability under-approximation",
                "snippet":['model.saturated_lcg("a=1")']},
            {"name": "Worth LCG for goal-oriented model reduction",
                "snippet":['model.worth_lcg("a=1")']}
            ]},
        "---",
        {"name":"State graph analysis",
            "sub-menu": [
            {"name": "Count reachable states",
                "snippet":['model.count_reachable_states()']},
            {"name": "Reachable state graph",
                "snippet":['model.reachable_stategraph()']},
            {"name": "Reachable attractors",
                "snippet":['model.reachable_attractors()']},
            {"name": "Fixpoints",
                "snippet":['model.fixpoints()']}
            ]},
        "---",
        {"name":"Goal specification",
            "sub-menu": [
            {"name": "Simple goal", "snippet":['"a=1"']},
            {"name": "Sub-state goal", "snippet":['"a=1,b=1"']},
            {"name": "Sequence of simple goals", "snippet":['"a=1","b=1"']},
            {"name": "Sequence of sub-state goals", "snippet":['"a=1,c=1","b=1,d=0"']},
            {"name": "Alternative goals", "snippet":['pypint.Goal("a=1")|pypint.Goal("b=1")']}
            ]}
    ]
    toolbar = [
        {"name": "upload", "setup": {
            "icon": "fa-upload",
            "help": "Upload model",
            "handler": "action_upload_model"}},
        {"name": "enable-debug", "setup": {
            "help": "Enable debug",
            "handler": "action_enable_debug"}},
        {"name": "disable-debug", "setup": {
            "help": "Disable debug",
            "handler": "action_disable_debug"}},
    ]


    js_api = {
    "action_upload_model": """function() {
        var cell = Jupyter.notebook.get_selected_cell();
        cell.set_text('model = '+pypint_jsapi.module_alias+'.load()');
        cell.focus_editor();
    }""",
    "action_enable_debug": """function() {
        IPython.notebook.kernel.execute(pypint_jsapi.module_alias+".enable_dbg()");
        pint_jsapi.debug_enabled(true);
    }""",
    "action_disable_debug": """function() {
        IPython.notebook.kernel.execute(pypint_jsapi.module_alias+".disable_dbg()");
        pint_jsapi.debug_enabled(false);
    }""",
    "btn_enable_debug": "null",
    "btn_disable_debug": "null",
    "debug_enabled": """function(enabled) {
        if (enabled) {
            this.btn_enable_debug.hide();
            this.btn_disable_debug.show();
        } else {
            this.btn_enable_debug.show();
            this.btn_disable_debug.hide();
        }
    }""",
    "post_install_callback": """function() {
        this.btn_enable_debug = $("#pypint-toolbar > button[data-jupyter-action='pypint:enable-debug']");
        this.btn_disable_debug = $("#pypint-toolbar > button[data-jupyter-action='pypint:disable-debug']");
        this.btn_enable_debug[0].innerHTML = "enable debug";
        this.btn_disable_debug[0].innerHTML = "disable debug";
        this.debug_enabled(%s);
    }""" % (1 if CFG["dbg"] else 0)
    }

    jupyter_setup(__name__, label="Pint",
        color="red",
        menu=menu,
        toolbar=toolbar,
        js_api=js_api)

