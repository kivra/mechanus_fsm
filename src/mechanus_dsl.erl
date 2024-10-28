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
%%% @doc External DSL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(mechanus_dsl).

%%%_* Exports ==========================================================
-export([parse/1]).
-export([visualize/1]).

%%%_* Includes =========================================================
-include("mechanus.hrl").

%%%_* Code =============================================================
-spec parse(filename()) -> 'maybe'([_], _).
parse(File) when is_atom(File) ->
    parse(?a2l(File) ++ ".mrl");
parse(File) when ?is_string(File) ->
  s2_maybe:do(
    [ ?thunk(file:open(File, [read]))
    , fun read_lines/1
    , fun mechanus_lexer:lex/1
    , fun mechanus_parser:parse/1
    , fun rewrite/1
    ]).

read_lines(FD) ->
  read_lines(file:read_line(FD), "", FD).
read_lines(eof, Lines, FD) ->
  ok = file:close(FD),
  Lines;
read_lines({ok, Line}, Lines, FD) ->
  read_lines(file:read_line(FD), Lines ++ Line, FD).

%% Translate to mechanus_modron's internal format.
rewrite(ParseTree) ->
  [list_to_tuple(
     [State, EntryActions, ExitActions] ++
       lists:flatmap(fun erlang:tuple_to_list/1, Transitions)) ||
    {State, {EntryActions, ExitActions}, Transitions} <- ParseTree].

visualize(MrlFile) ->
  {ok, Parsed} = parse(MrlFile),
  Name         = filename:rootname(MrlFile),
  DotFile      = Name ++ ".dot",
  Vertices     = get_vertices(Parsed),
  Edges        = get_edges(Parsed),
  s2_dot:digraph({filename:basename(Name), Vertices, Edges}, DotFile),
  {ok, _}      = s2_sh:eval(io_lib:format("dot -Tpng -O ~s", [DotFile])),
  ok.

get_vertices(Parsed) ->
  [{?a2l(State), lists:flatten(
            [ io_lib:format("~s\\n", [State])
            , lists:map(fun (Item) -> io_lib:format("~p\\l", [Item]) end, Ens ++ Exs)
            ])} ||
     [State, Ens, Exs|_] <- [?t2l(X) || X <- Parsed]].

get_edges(Parsed) ->
  lists:flatmap(
    fun(X) ->
      [State, _, _|Transitions] = ?t2l(X),
      [{?a2l(State), ?a2l(NextState), ?a2l(Event)} ||
        [Event, NextState] <- s2_lists:partition(2, Transitions)]
    end, Parsed).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
  {ok, [ {'MY_STATE', [foo, bar], [baz],
          some_event, 'YOUR_STATE',
          another_event, 'HER_STATE'}
       , {'YOUR_STATE', [], [quux],
          yae, 'HER_STATE'}
       , {'HER_STATE', [], []}
       ]} = parse(relative_to_this_file("../test/test.mrl")),
  %% Lexer error (; instead of ,)
  {error, _} = parse(list_to_atom(relative_to_this_file("../test/test_bad"))).

relative_to_this_file(Path) ->
  filename:join(filename:dirname(?FILE), Path).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
