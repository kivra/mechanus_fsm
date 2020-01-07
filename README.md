![Tridrone](https://upload.wikimedia.org/wikipedia/en/5/56/Tridrone.JPG)

Mechanus (v0.1.0)
========
Mechanus is a system for defining and running, event-driven
state machines.

A state machine consists of a set of states. Each state is associated
with a list of entry actions, a list of exit actions, and a transition
table.
Actions are application-specific and opaque to Mechanus.
Transition tables are indexed by events, which are either sent to
Mechanus or the result of an action.

State machines are described in a simple DSL. State machines are run
by first creating an instance from a DSL-template, then sending
events to that instance. Events cause state transitions, which may cause
actions to be fired.

In the standard D&D cosmology, Mechanus is the plane of existence
associated with the Lawful Neutral alignment.
Mechanus is home to a race of sentient automatons known as Modrons.

Concepts
--------
* State machine: an abstract description of some process
* Modron: a running instance of a state machine
* Event: a message sent to some Modron
* Transition: a state change in some Modron caused by some event
* Action: an effect associated with a transition
* Mid: ModronID
* MSL: Modron Specification Language, the DSL used to describe state
machines

MSL
---
Whitespace may be used at will. Comments begin with a '%'.
MSL filenames should end in '.mrl'.

An MSL program is a sequence of state declarations.
A state declaration is a state name optionally followed by entry
actions, exit actions, and transitions.
Actions is a list of names of the corresponding erlang callback-modules. Actions
are evaluated in order left to right with each result merged with the previous.
Transitions are described as an event name, followed by an arrow,
followed by a state name.
State names are upper case, event and action names are lower case.

Example:

    START: go -> FIRST_STATE.

    FIRST_STATE:
      EntryActions my_action.

      event_1 -> SECOND_STATE,
      event_2 -> THIRD_STATE.

    SECOND_STATE:
      ExitActions your_action.
      event_3 -> THIRD_STATE.

    THIRD_STATE: . %halting state

More formally, the keywords are ':' and '.' to delimit states,
'EntryActions' or 'ExitActions' and '.' to delimit action lists, and '->'
and ',' to delimit transitions.

See mechanus_parser.yrl for a formal grammar.

Event scheduling
----------------
Events are either injected (returned by an action) or sent (via
`send/2`). Events are either immediate (valid_from = 0) or future
(valid_from > 0).
Events are processed in validity order (in particular, immediate
events are _always_ processed before future events).
Ties are broken as follows. Injected events are processed before
sent events. If event A was injected/sent before event B, event
A will be processed before event B.

TODO
----
  - Allow loops in FSMs.
