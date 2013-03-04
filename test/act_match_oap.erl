-module(act_match_oap).
-behaviour(mechanus_action).
-export([perform/1]).
-include_lib("mechanus_fsm/include/mechanus.hrl").
perform(_) ->
    #result{events=[s2_rand:pick([ #event{name=match_oap_yes}
                                 , #event{name=match_oap_no}
                                 ])]}.
