---
layout: default
title: Syntax for Automata Network (.an) files
category_title: Documentation
---

Automata Network source file has a filename ending with `.an`.

An Automata Network consists in finite-state machines (automata) declared by a
name with a finite set of local states, and a by a list of transitions.

### Automaton declaration

An automaton is defined by a name and by a list of local states:

<p>
<code>
<a href="t_name" class="type">Name</a>
[
<a href="t_state" class="type">State</a>,
</code>...<code>,
<a href="t_state" class="type">State</a>
]
</code>
</p>

*Examples*

```
a [0, 1] (* automaton "a" with two local states *)
"a" [0, 1] (* equivalent to previous declaration *)
b [0, 1, "stalled"]
"Fyn-1" ["inactive", "active"]
```

### Transition declaration

A transition is local to one automaton and can be conditioned with the states of
any other (previously) declared automata.

<p>
<code>
<a href="t_name" class="type">Name</a>
<a href="t_state" class="type">State</a>
->
<a href="t_state" class="type">State</a>
<span class="syn_opt">(</span>
when
<a href="t_name" class="type">Name</a>=<a href="t_state" class="type">State</a>
and
</code>...<code>
<a href="t_name" class="type">Name</a>=<a href="t_state" class="type">State</a>
<span class="syn_opt">)</span>
</code>
</p>

Whenever the specified local state is an integer, it refers to the identifier of
the local state of the automata, i.e., its index in the list of declared local
states.


*Examples*

```
a 0 -> 1 when "Fyn-1"="active" and b=2 (* b="stalled" *)
a 1 -> 0  (* no external condition *)
"Fyn-1" "inactive" -> "active" when a=0
"Fyn-1" 0 -> 1 when a=0 (* equivalent to previous declaration *)
```

### Initial state

By default, each automaton start at the local state <code>0</code>.
The initial state of the automata network can be override with the following
directive:

<p>
<code>initial_state 
<a href="t_name" class="type">Name</a>=<a href="t_state" class="type">State</a>,
</code>...<code>
<a href="t_name" class="type">Name</a>=<a href="t_state" class="type">State</a>
</code>
</p>


### Comments

Comments are enclosed by `(*` and `*)` and can be nested.


#### Data types
<dl>
<dt><code><a name="t_name"></a>Name</code></dt>
<dd>A name is either a string matching <code>(A..z_)(A..z0..9_')*</code>
or any string enclosed with <code>"</code></dd>
<dt><code><a name="t_state"></a>State</code></dt>
<dd>A state is either an integer or a
<code><a href="t_name" class="type">Name</a></code>.
</dd>
</dl>

