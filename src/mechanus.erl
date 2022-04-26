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
%%% @doc API.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(mechanus).

%%%_* Exports ==========================================================
%% Constructors
-export([event/1]).
-export([event/2]).
-export([event/3]).
-export([result/0]).
-export([result/1]).
-export([result/2]).
-export([result_to_map/1]).

%% Time
-export([in/1]).
-export([at/1]).
-export([now/0]).
-export([seconds/1]).
-export([minutes/1]).
-export([hours/1]).
-export([days/1]).

-export_type([data/0]).
-export_type([event/0]).
-export_type([id/0]).
-export_type([result/0]).
-export_type([spec/0]).

%%%_* Includes =========================================================
-include("mechanus.hrl").

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
%% Aliases
-type action()   :: module().

%% Inputs
-type data()     :: dict:dict()
                  | [{_, _}]
                  | eon:object()
                  | map().
-type id()       :: _.
-type spec()     :: [{atom(), atom(), atom()}
                  | {atom(), [action()], [action()]}]
                  | [{atom() | [action()]}].

%% ADTs
-opaque event()  :: #event{}.
-opaque result() :: #result{}.

%%%_ * Constructors ----------------------------------------------------
-spec event(atom()) -> event().
event(Name) ->
  event(Name, []).
-spec event(atom(), data()) -> event().
event(Name, Input)  ->
  event(Name, Input, 0).
-spec event(atom(), data(), non_neg_integer()) -> event().
event(Name, Input, ValidFrom)
  when is_atom(Name)
     , is_integer(ValidFrom)
     , ValidFrom >= 0 ->
  #event{name=Name, input=data(Input), valid_from=ValidFrom}.

-spec result() -> result().
result() ->
  result([]).
-spec result(data()) -> result().
result(Output) ->
  result(Output, []).
-spec result(data(), event() | atom())   -> result()
          ; (data(), [event() | atom()]) -> result().
result(Output, Event) when not is_list(Event) ->
  result(Output, [Event]);
result(Output, Events) when is_list(Events) ->
  #result{ output = data(Output)
         , events = [if is_atom(E)          -> event(E);
                        is_record(E, event) -> E
                     end || E <- Events]
         }.

result_to_map(#result{output = O, events = E}) ->
  #{output => redact(O), events => E};
result_to_map(R) ->
  R.

redact(#{company := Company}) ->
  eon:del(eon:del(eon:del(Company, <<"permissions">>),
                  <<"shares">>),
          <<"signatories">>);
redact(O) ->
  O.

data(Data) when is_map(Data) ->
  Data;
data(Data) ->
  eon:new(Data).

%%%_ * Time ------------------------------------------------------------
%% @doc Return a valid_from time in the future.
%% Millisecond resolution.
in({N, seconds}) -> ?MODULE:now() + seconds(N);
in({N, minutes}) -> ?MODULE:now() + minutes(N);
in({N, hours})   -> ?MODULE:now() + hours(N);
in({N, days})    -> ?MODULE:now() + days(N).

at({Hr,Min,Sec}) ->
  Now       = {{Y,M,D}, {_,_,_}} = calendar:local_time(),
  Then      = {{Y,M,D}, {Hr,Min,Sec}},
  NowGSecs  = calendar:datetime_to_gregorian_seconds(Now),
  ThenGSecs = calendar:datetime_to_gregorian_seconds(Then),
  DiffSecs  = ThenGSecs - NowGSecs,
  ?MODULE:now() + lists:max([DiffSecs, 0]) * 1000.

%% @doc Current time in ms.
now()      -> s2_time:stamp() div 1000.

%% @doc N seconds in ms.
seconds(N) -> timer:seconds(N).

%% @doc N minutes in ms.
minutes(N) -> timer:minutes(N).

%% @doc N hours in ms.
hours(N)   -> timer:hours(N).

%% @doc N days in ms.
days(N)    -> timer:hours(24) * N.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

types_test() ->
  Event  = event(ev1),
  _      = result(),
  _      = result([], foo),
  _      = result([], Event).

time_test() ->
  _    = in({10, seconds}),
  true = in({2, days})  > in({2, hours}),
  true = in({2, hours}) > in({2, minutes}).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
