-module(act_exactly_one_match).
-behaviour(mechanus_action).
-export([perform/1]).
-include_lib("mechanus_fsm/include/mechanus.hrl").
perform(_) ->
    #result{events=[s2_rand:pick([ #event{name=exactly_one_match_yes}
                                 , #event{name=exactly_one_match_no}
                                 ])]}.
