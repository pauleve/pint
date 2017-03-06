
import networkx as nx
from .ui import *

def svg_of_graph(g):
    """
    Returns SVG representation of graph `g` with GraphViz dot layout.
    """
    dbg("computing graph layout...")
    return nx.nx_pydot.to_pydot(g).create_svg().decode()

def install_default_formatters():
    """
    Register default IPython formatters:

    * ``nx.Graph`` with :py:func:`.svg_of_graph`
    """
    ip = get_ipython()
    # nxgraph to svg
    svg_formatter = ip.display_formatter.formatters["image/svg+xml"]
    svg_formatter.for_type(nx.Graph, svg_of_graph)


def ipython_install():
    """
    Installs IPython user interface:

    * :py:func:`.install_default_formatters`
    """
    install_default_formatters()


