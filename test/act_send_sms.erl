-module(act_send_sms).
-behaviour(mechanus_action).
-export([perform/1]).
-include_lib("mechanus_fsm/include/mechanus.hrl").
perform(_) -> #result{events=[#event{name=send_sms_ok}]}.
