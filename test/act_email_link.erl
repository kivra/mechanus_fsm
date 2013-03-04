-module(act_email_link).
-behaviour(mechanus_action).
-export([perform/1]).
-include_lib("mechanus_fsm/include/mechanus.hrl").
perform(_) -> #result{events=[#event{name=email_link_ok}]}.
