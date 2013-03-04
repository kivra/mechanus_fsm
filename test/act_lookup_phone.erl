-module(act_lookup_phone).
-behaviour(mechanus_action).
-export([perform/1]).
-include_lib("mechanus_fsm/include/mechanus.hrl").
perform(_) ->
    #result{events=[s2_rand:pick([ #event{name=lookup_phone_yes}
                                 , #event{name=lookup_phone_no}
                                 ])]}.
