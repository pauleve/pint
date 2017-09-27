:part: doc

.. _doc-automata-networks:

Automata networks
-----------------

Pint takes as input automata networks.
Automata networks are transition-centered models, close to 1-bounded Petri nets.
They gather a finite number of finite-state machines, where their internal transitions can be conditioned by the state of other automata in the network.

Pint uses a simple plain text format to specify automata networks and their initial state. The text files have the extension ``.an``.

Models expressed as Boolean and multi-valued networks can be automatically converted using the python interface.
See :py:func:`pypint.model.load` and :doc:`model` tutorial.

The structure of a ``.an`` file is the following.
You can refer to the `model repository <https://github.com/pauleve/pint/tree/master/examples>`_ for some examples.

* Automata declaration

An automaton is defined by a name and a list of local states.
If the name of the automaton contain special characters, it should be enclosed with `"`.
The local states can either be integers, or strings (enclosed in `"`).

Examples:

::

    a [0, 1]
    b [0, 1, 2]
    "Fyn-1" ["inactive", "active"]


* Transitions

A transition specifies a local state change within one automaton, and can be conditionned by the conjunction of states of other automata.

Examples:

::

    a 0 -> 1 when "Fyn-1"="active" and b=2
    a 1 -> 0  (* no external condition *)
    "Fyn-1" "inactive" -> "active" when a=0
    "Fyn-1" 0 -> 1 when a=0 (* equivalent to previous declaration *)

Transitions can also be coupled, i.e., their application is done simultaneously:

::

    { b 0 -> 1 ; "Fyn-1" "active" -> "inactive" } when a = 1

The above synchronized transition can be perfomed only when `a=1`, `b=0`, and `Fyn-1=active`.


* Initial state

By default, each automaton starts in the local state `0`. The initial state of the automata network can be overrided with the following
directive:

::

    initial_state a=1,b=2

