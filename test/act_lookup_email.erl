-module(act_lookup_email).
-behaviour(mechanus_action).
-export([perform/1]).
-include_lib("mechanus_fsm/include/mechanus.hrl").
perform(_) ->
    #result{events=[s2_rand:pick([ #event{name=lookup_email_yes}
                                 , #event{name=lookup_email_no}
                                 ])]}.
