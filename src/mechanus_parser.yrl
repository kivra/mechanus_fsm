%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2015 Kivra
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc MSL grammar.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals spec state actions entry_actions exit_actions lcids transitions transition.
Terminals 'EntryActions' 'ExitActions' lcid ucid ':' '->' ',' '.'.
Rootsymbol spec.
Endsymbol '$end'.

%% A modron is specified by its states.
spec          -> state                            : ['$1'].
spec          -> state spec                       : ['$1'|'$2'].

%% A state has a name, some actions and some transitions.
state         -> ucid ':' actions transitions '.' : {unwrap('$1'), '$3', '$4'}.

actions       -> '$empty'                         : {[],   []}.
actions       -> entry_actions                    : {'$1', []}.
actions       -> exit_actions                     : {[],   '$1'}.
actions       -> entry_actions exit_actions       : {'$1', '$2'}.
actions       -> exit_actions entry_actions       : {'$2', '$1'}.

entry_actions -> 'EntryActions' lcids '.'         : '$2'.
exit_actions  -> 'ExitActions'  lcids '.'         : '$2'.

lcids         -> lcid                             : [unwrap('$1')].
lcids         -> lcid lcids                       : [unwrap('$1')|'$2'].

transitions   -> '$empty'                         : [].
transitions   -> transition                       : ['$1'].
transitions   -> transition ',' transitions       : ['$1'|'$3'].

transition    -> lcid '->' ucid                   : {unwrap('$1'), unwrap('$3')}.


Erlang code.

-spec unwrap({_, integer(), _}) -> _.
%% @doc Extract content from a Leex token.
unwrap({_, _, Content}) -> Content.

%%% eof
