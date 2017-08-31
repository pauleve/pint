:part: index

Introduction
------------

**Pint** implements static analyses for computing dynamical properties on
**very large-scale Automata Networks**, ranging from hundreds to
several thousands of concurrently interacting components.
Provided analyses include notably the listing of fixed points,
**successive reachability properties, cut sets and mutations for reachability, and model reduction
preserving transient dynamics**.
The translation to related formalisms, in particular Boolean and multi-valued networks, is also
provided.

Automata Networks are defined by a set of finite-state machines whose local
transitions can be conditionned by the state of other automata in the network.
Applications are in particlar in **systems biology** with the modelling and
analysis of signalling pathways and gene regulatory networks, gathering multiple
interacting components with a few local states.

Pint comes with several command line tools to perform formal analyses,
reductions, simulations, and translation to other formalisms.
A Python interface, and seamless integration with Jupyter notebook is available.
An OCaml library, with possible C bindings, can also be compiled in order to
embed the static analyses in other frameworks.


Documentation
=============
.. toctree::
   :maxdepth: 2

   doc/index

Model repository
================

`Model repository <https://github.com/pauleve/pint/tree/master/examples>`_ on
GitHub.


Related Papers
==============

- `Goal-Driven Unfolding of Petri Nets <https://hal.archives-ouvertes.fr/hal-01392203/file/godunf.pdf>`_
  (T. Chatain, and L. Paulevé at *CONCUR 2017*)
- `Identification of bifurcation transitions in biological regulatory networks using Answer-Set Programming <http://dx.doi.org/10.1186/s13015-017-0110-3>`_
  (L. F. Fitime,  O. Roux, C. Guziolowski, and L. Paulevé in *Algorithms for Molecular Biology*, 2017)
- `Goal-Oriented Reduction of Automata Networks <https://hal.archives-ouvertes.fr/hal-01149118/file/gored.pdf>`_ (L. Paulevé at *CMSB 2016*)
- `Sufficient conditions for reachability in automata networks with priorities <https://hal.archives-ouvertes.fr/hal-01202671/file/main.pdf>`_
  (M.  Folschette, L. Paulevé, M. Magnin, and O. Roux in *Theoretical Computer Science (TCS)*, 2015)
- `Analyses statiques de la dynamique des réseaux d'automates indéterministes
  <https://hal.archives-ouvertes.fr/hal-01070295v2/document>`_ (L. Paulevé, M. Folschette, M. Magnin, O. Roux in
  *Technique et Science Informatiques (TSI)*, 2015)
- `Identification of biological regulatory networks from Process Hitting models <https://hal.archives-ouvertes.fr/hal-01094249/file/main.pdf>`_
  (M.  Folschette, L. Paulevé, K. Inoue, M. Magnin, and O. Roux in *Theoretical Computer Science (TCS)*, 2015)
- `Analyzing Large Network Dynamics with Process Hitting
  <https://hal.archives-ouvertes.fr/hal-01060490/file/PCFMR14-chapterLMBS.pdf>`_ (L. Paulevé, C.
  Chancellor, M. Folschette, M. Magnin, and O. Roux chapter of *Logical Modeling of
  Biological Systems* book.)
- `Under-Approximating Cut Sets for Reachability in Large Scale Automata Networks
  <https://hal.archives-ouvertes.fr/hal-00769447v3/document>`_
  (L. Paulevé, G. Andrieux, H. Koeppl at *Computer Aided Verification (CAV)*, 2013)
- `Static analysis of biological regulatory networks dynamics using abstract
  interpretation <http://loicpauleve.name/PMR12-MSCS.pdf>`_ (L. Paulevé, M. Magnin, O. Roux in *Mathematical Structures in
  Computer Science (MSCS)*, 2012)
- `Refining Dynamics of Gene Regulatory Networks in a Stochastic π-Calculus <http://hal.archives-ouvertes.fr/hal-00397235>`_ (L.
  Paulevé, M. Magnin, O. Roux in *Transactions on Computational Systems Biology
  (TCSB)*, 2011)

Authors and contributors
========================

Pint has been created and is maintained by `Loïc Paulevé <http://loicpauleve.name>`_.
It also contains contributions from `Maxime Folschette <http://maxime.folschette.name/>`_
(inference of interaction graph and Boolean networks).

Support and contact
===================

If you have any question regarding Pint, please contact loic.pauleve@lri.fr
or open an issue on `Pint GitHub <https://github.com/pauleve/pint/issues>`_.

