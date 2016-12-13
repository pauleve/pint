
import networkx as nx

from IPython.display import display, Image

def idraw_graph(g):
    display(Image(nx.nx_pydot.to_pydot(g).create_png()))


