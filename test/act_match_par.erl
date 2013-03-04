-module(act_match_par).
-behaviour(mechanus_action).
-export([perform/1]).
-include_lib("mechanus_fsm/include/mechanus.hrl").
perform(_) ->
    #result{events=[s2_rand:pick([ #event{name=match_par_yes}
                                 , #event{name=match_par_no}
                                 ])]}.
