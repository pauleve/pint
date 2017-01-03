
import networkx as nx
from .ui import *

def svg_of_graph(g):
    info("# computing graph layout...")
    return nx.nx_pydot.to_pydot(g).create_svg().decode()

def install_default_formatters():
    ip = get_ipython()
    # nxgraph to svg
    svg_formatter = ip.display_formatter.formatters["image/svg+xml"]
    svg_formatter.for_type(nx.MultiDiGraph, svg_of_graph)


def ipython_install():
    install_default_formatters()


