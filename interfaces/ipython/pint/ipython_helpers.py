
import os

import networkx as nx
from .cfg import *
from .ui import *

from IPython.display import display, HTML

def svg_of_graph(g):
    """
    Returns SVG representation of ``networkx.Graph`` `g` with GraphViz dot layout.
    """
    dbg("computing graph layout...")
    return nx.nx_pydot.to_pydot(g).create_svg().decode()

def install_default_formatters():
    """
    Register default IPython formatters:

    * ``networkx.Graph`` with :py:func:`.svg_of_graph`
    """
    ip = get_ipython()
    # nxgraph to svg
    svg_formatter = ip.display_formatter.formatters["image/svg+xml"]
    svg_formatter.for_type(nx.Graph, svg_of_graph)

def jupyter_js(data, args=""):
    return """<script type="text/javascript" %s>
        if (typeof Jupyter != 'undefined') {
            %s }</script>""" % (args, data)

def disp_jupyter_js(data):
    display(HTML(jupyter_js(data)))

def jupyter_extension():
    """
    Customize Jupyter notebook interface.
    """
    jsfile = os.path.join(os.path.dirname(__file__), "ipython_ext.js")
    cssfile = os.path.join(os.path.dirname(__file__), "ipython_ext.css")
    with open(cssfile) as f:
        css = """<style type="text/css">%s</style>""" % f.read()
    with open(jsfile) as f:
        js = jupyter_js("%s pint_ui_debug_enabled(%s)" % \
            (f.read(), "true" if CFG["dbg"] else "false"),
            'class="to-be-removed"')
    display(HTML("%s%s" % (css, js)))

def ipython_install():
    """
    Installs IPython user interface:

    * :py:func:`.install_default_formatters`
    * :py:func:`.jupyter_extension`
    """
    install_default_formatters()
    jupyter_extension()

