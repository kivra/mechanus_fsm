-module(act_match_eniro).
-behaviour(mechanus_action).
-export([perform/1]).
-include_lib("mechanus_fsm/include/mechanus.hrl").
perform(_) ->
    #result{events=[s2_rand:pick([ #event{name=match_eniro_yes}
                                 , #event{name=match_eniro_no}
                                 ])]}.
