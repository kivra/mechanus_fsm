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
%%% @doc State machines!
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(mechanus_modron).
-compile({no_auto_import, [apply/2]}).

%%%_* Exports ==========================================================
-export([eval/2]).
-export([eval/3]).
-export([apply/2]).

%%%_* Includes =========================================================
-include("mechanus.hrl").

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec eval(filename() | mechanus:spec(), mechanus:id()) -> maybe(#modron{}, _).
%% @doc Construct modron ID according to Spec.
eval(Source, Mid) ->
  eval(Source, Mid, []).
eval(File, Mid, Data) when is_binary(File) ->
  eval(binary_to_list(File), Mid, Data);
eval(File, Mid, Data)
  when is_atom(File)
     ; ?is_string(File) ->
  {ok, Spec} = mechanus_dsl:parse(File),
  eval(Spec, Mid, Data);
eval(Spec, Mid, Data)
  when is_list(Spec) ->
  s2_maybe:do(
    [ ?thunk(#modron{id=Mid, state=parse(Spec), data=data(Data), spec=Spec})
    , fun(#modron{state=S} = M) -> M#modron{actions=enter_state(S)} end
    ]).

data(Data) when is_map(Data) ->
  Data;
data(Data) ->
  eon:new(Data).

-spec apply(#modron{}, #event{} | [#event{}]) ->
               maybe({done, #modron{}} |
                     {next, #modron{}} |
                     {next, #modron{}, timeout()},  _).
%% @doc Send Events to Modron, then take as many steps as possible.
apply(Modron, Events) ->
  s2_maybe:do(
    [ ?thunk(reduce(Modron, Events))
    , fun status/1
    ]).

reduce(Modron, Events) when not is_list(Events) ->
  reduce(Modron, [Events]);
reduce(Modron, Events) ->
  s2_funs:fix(
    fun(Modron) -> effect(transition(Modron)) end,
    lists:foldl(s2_funs:flip(fun send/2), Modron, Events),
    fun(#modron{events=Es1}, #modron{events=Es2}) -> Es1 =:= Es2 end).

status(#modron{state=S, events=Es} = M) ->
  case is_halting_state(S) of
    true                 -> {done, M};
    false when Es =:= [] -> {next, M};
    false                -> {next, M, (hd(Es))#event.valid_from}
  end.

is_halting_state(#state{tab=Tab}) -> eon:size(Tab) =:= 0.

%%%_ * DSL -------------------------------------------------------------
%% Mechanus accepts modron specifications in two forms:
%%
%% 1) Row-based:
%%
%%   [ {s0, [act1, ...], [...]} %action spec
%%   , {s0, event0, s1}         %transition spec
%%   , {s0, event1, s2}         %transition spec
%%   ...
%%   ]
%%
%% The order of tuples doesn't matter.
%%
%% 2) State-based:
%%
%%   [ { s0
%%     , [act1, ...], [...] %actions
%%     , event0, s1         %\
%%     , event1, s2         % }transitions
%%     ...                  %/
%%     }
%%   ...
%%   ]
%%
%% The order of tuples (and transitions within tuples) doesn't matter,
%% but tuples must start with the state-name and up to two action-lists.
%% The first action list denotes entry actions, the second exit actions.
parse(Xs) ->
  case lists:usort([size(X) || X <- Xs]) of %meh
    [3] -> parse_rows(Xs);
    _   -> parse_rows(states2rows(Xs))
  end.

states2rows(Xs) ->
  lists:flatmap(
    fun(X) ->
      case ?t2l(X) of
        [S, En, Ex|Trans]
          when is_list(En)
             , is_list(Ex) -> [{S, En, Ex}] ++ transitions2rows(S, Trans);
        [S, En|Trans]
          when is_list(En) -> [{S, En, []}] ++ transitions2rows(S, Trans);
        [S|Trans]          -> [{S, [], []}] ++ transitions2rows(S, Trans)
      end
    end, Xs).

transitions2rows(S0, Trans) ->
  ?hence(length(Trans) rem 2 =:= 0),
  [{S0, Event, S1} || [Event, S1] <- s2_lists:partition(2, Trans)].

%% Build state-graph by:
%% 1) obtaining dependency order of states via topological sort of graph
%%    induced by transitions,
%% 2) using this order, building states starting from the end states
%%    (this way, state-names occuring in predecessor edges are
%%    guaranteed to point to existing states),
%% 3) returning the start state.
parse_rows(Rows) ->
  Order  = kahn_sort(rows2graph(Rows)),
  States = build_states(Rows, Order),
  ?unlift(eon:get(States, lists:last(Order))).


rows2graph(Rows) ->
  lists:usort([{S0, S1} || {S0, _, S1} <- Rows, is_atom(S1)]).

%% @doc Topological sort, c.f. Wikipedia.
kahn_sort(Graph) ->
  Preds = lists:usort([S || {S, _} <- Graph]),
  Succs = lists:usort([S || {_, S} <- Graph]),
  Roots = Preds -- Succs,
  [_]   = Roots, %we expect exactly one start node
  kahn_loop(Graph, Roots, []).

kahn_loop(Graph0, [Pred|Preds0], Sorted) ->
  {Succs0, Graph} = lists:partition(fun({S0, _S1}) -> S0 =:= Pred end, Graph0),
  Succs = [S || {_, S} <- Succs0],
  Preds = Preds0 ++ [S || S <- Succs, [X || {_, X} <- Graph, X =:= S] =:= []],
  kahn_loop(Graph, Preds, [Pred|Sorted]);
kahn_loop([], [], Sorted) -> Sorted.


build_states(Rows, Order) ->
 lists:foldl(
   fun(Name, States) ->
     [{Name, OnEntry, OnExit}|Transitions] = get_rows(Rows, Name),
     eon:set(
       States,
       Name,
       #state{ name     = Name
             , tab      = build_tab(Transitions, States)
             , on_entry = OnEntry
             , on_exit  = OnExit
             })
   end, eon:new(), Order).

get_rows(Rows, Name) ->
  %% Lists > Atoms
  lists:sort(fun(X, Y) ->  X > Y end,
             [R || {S, _, _} = R <- Rows, S =:= Name]).

build_tab(Transitions, States) ->
  lists:foldl(
    fun({_, Event, Name}, Tab) ->
      eon:set(Tab, Event, ?unlift(eon:get(States, Name)))
    end, eon:new(), Transitions).

%%%_ * Engine ----------------------------------------------------------
-spec send(#modron{}, #event{}) -> #modron{}.
%% @doc Put E on M's event queue.
send(#modron{events=Es} = M, E) -> M#modron{events=Es ++ [E]}.

-spec transition(#modron{}) -> #modron{}.
%% @doc Take the transition associated with the next event.
transition(#modron{events=[]} = M) ->
  M;
transition(#modron{ state   = S0
                  , data    = D0
                  , events  = Es
                  , ev_hist = Hist
                  , actions = As1
                  } = M) ->
  [E|Rest] = next_event(Es, Hist),
  case is_valid(E) of
    true ->
      {S, As2} = exit_state(S0, E#event.name, M#modron.id),
      As3      = enter_state(S),
      D        = merge(D0, E#event.input),
      M#modron{ state   = S
              , data    = D
              , events  = Rest
              , ev_hist = [E|Hist]
              , actions = As1 ++ As2 ++ As3
              };
    false ->
      M#modron{events=[E|Rest]}
  end.

%% Event scheduling
%% ----------------
%% Events are either injected (returned by an action) or sent (via
%% send/2). Events are either immediate (valid_from = 0) or future
%% (valid_from > 0).
%% Events are processed in validity order (in particular, immediate
%% events are _always_ processed before future events).
%% Ties are broken as follows. Injected events are processed before
%% sent events. If event A was injected/sent before event B, event
%% A will be processed before event B.
%%
%% We do not re-apply events from ev_hist. Unexpected events are treated
%% as errors.
next_event(Es, Hist) ->
  lists:keysort(#event.valid_from, dedup(Es, Hist)). %stable!

dedup(Es, Hist) ->
  %% We assume that duplicates are due to crashes.
  [E || E <- Es, [] =:= [H || H <- Hist,
                              H#event.id =:= E#event.id,
                              E#event.id =/= '']].


is_valid(#event{valid_from=TS}) -> mechanus:now() > TS.

-spec exit_state(#state{}, atom(), mechanus:id()) ->
                    {#state{}, [module()]} | no_return().
%% @doc Go to the next state via Event.
exit_state(#state{name=Name, tab=Tab, on_entry=OnEntry, on_exit=OnExit}, Event, ID) ->
  case eon:get(Tab, Event) of
    {ok, State} ->
      update_counter(mechanus_state_transitions, [Name, State#state.name, Event]),
      ?debug(#{ description => "State transition applied"
             , action_id => ID
             , action_name => Name
             , state => State#state.name
             , event => Event
             }),
      {State, OnExit};
    {error, notfound} ->
      update_counter(mechanus_state_transitions_failed, [Name, Event]),
      ?error(#{ description => "no transition found for action"
              , action_id => ID
              , action_name => Name
              , event => Event
              }),
      throw({error, {no_such_transition,
            [ {modron_id, ID}
            , {modron_state, Name}
            , {modron_entry_actions, OnEntry}
            , {modron_exit_actions, OnExit}
            , {modron_transition_to, Event}
            ] }})
  end.

enter_state(#state{on_entry=OnEntry}) -> OnEntry.

-spec effect(#modron{}) -> #modron{} | no_return().
%% @doc Perform all pending actions.
effect(#modron{id=ID, actions=As, act_hist=Hist} = M) ->
  lists:foldl(
    fun(A, #modron{data=D, events=Es} = M) ->
      case ?lift(A:perform(D)) of
        {ok, R} ->
          update_counter(mechanus_actions, [A, success]),
          ?debug(#{ description => "Action succeeded"
                  , action_id => ID
                  , action_name => A
                  }),
          %% Inject before existing events!
          M#modron{data=merge(D, R#result.output), events=R#result.events++Es};
        {error, Rsn} = Err ->
          update_counter(mechanus_actions, [A, failure]),
          ?error(#{description => "Action failed"
                 , action_id => ID
                 , action_name => A
                 , reason => Rsn
                 }),
          throw(Err)
      end
    end, M#modron{actions=[], act_hist=[As|Hist]}, As).

update_counter(Name, Args) ->
  case mechanus:is_counters_initialized() of
    true ->
      prometheus_counter:inc(Name, Args),
      ok;
    false ->
      ok
  end.

-spec merge(mechanus:data(), mechanus:data()) -> mechanus:data().
%% @doc Merge Data2 into Data1, with entries in Data2 taking precedence.
merge(Data1, Data2) when is_map(Data1), is_map(Data2) ->
  maps:merge(Data1, Data2);
merge(Data1, Data2) ->
  eon:fold(
    fun(K, V, Data1) ->
      eon:set(Data1, K, V)
    end, Data1, Data2).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_fsm() ->
  [ {lookup_email, [act_lookup_email], [],
     lookup_email_yes, validate_phone,
     lookup_email_no,  lookup_phone}

  , {validate_phone, [act_validate_phone],
     validate_phone_yes, email_link,
     validate_phone_no,  print}

  , {email_link, [act_email_link],
     email_link_ok,    done_ok,
     email_link_error, done_error}

  , {print, [act_print],
     print_ok,    done_ok,
     print_error, done_error}

  , {lookup_phone, [act_lookup_phone],
     lookup_phone_yes, match_par,
     lookup_phone_no,  match_oap}

  , {match_par, [act_match_par],
     match_par_yes, send_sms,
     match_par_no,  match_eniro}

  , {send_sms, [act_send_sms],
     send_sms_ok,    done_ok,
     semd_sms_error, done_error}

  , {match_eniro, [act_match_eniro],
     match_eniro_yes, send_sms,
     match_eniro_no,  print}

  , {match_oap, [act_match_oap],
     match_oap_yes, exactly_one_match,
     match_oap_no,  match_eniro}

  , {exactly_one_match, [act_exactly_one_match],
     exactly_one_match_yes, send_sms,
     exactly_one_match_no,  print}

  , {done_ok}

  , {done_error}
  ].

parse_test() ->
  State        = parse(test_fsm()),
  lookup_email = State#state.name.

eval_apply_test() ->
  mechanus:init_counters(),
  {ok, M0}        = eval(test_fsm(), s2_time:stamp()),
  {ok, {done, M}} = apply(M0, []),
  done_ok         = (M#modron.state)#state.name.

test_fsm2() ->
  [ {s0, [], []}
  , {s0, ev1, s1}
  , {s0, ev2, s2}
  , {s1, [], [no_such_module]}
  , {s1, ev2, s2}
  , {s2, [], []}
  ].

parse_rows_test() ->
  State = parse(test_fsm2()),
  s0    = State#state.name.

special_case_test() ->
  %% Delayed event
  mechanus:init_counters(),
  T0 = mechanus:now(),
  T1 = T0 + 500,
  {ok, M0} = eval(test_fsm2(), T0, [{foo, bar}]),
  {ok, {next, M1, T1}} = apply(M0, #event{name=ev1, valid_from=T1}),
  timer:sleep(1000),
  {ok, {next, M}} = apply(M1, []),
  %% Bad event
  {error, {no_such_transition, _}} = apply(M, #event{name=ev3}),
  %% Bad Action
  {error, {lifted_exn, _, _}} = apply(M, #event{name=ev2}),

  ok.



merge_test() ->
  Obj     = merge([foo, 1], [foo, 2]),
  {ok, 2} = eon:get(Obj, foo).

merge_maps_test() ->
  Obj     = merge(#{foo => 1}, #{foo => 2}),
  {ok, 2} = maps:find(foo, Obj).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
