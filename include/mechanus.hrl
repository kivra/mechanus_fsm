%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Private header file.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Header ===========================================================
-ifndef(__MECHANUS_HRL).
-define(__MECHANUS_HRL, true).

%%%_* Includes =========================================================
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Macros ===========================================================
%%%_* Types ============================================================
%% A state machine is represented by its starting state.
-record(state,
        { name=error('state.name') :: atom()                %addresses
        , tab=error('state.tab')   :: obj(atom(), #state{}) %transitions
        , on_entry=[]              :: [module()]            %actions =
        , on_exit=[]               :: [module()]            % pred.on_exit +
        }).                                                 % succ.on_entry

%% Events advance the current state of a modron, which causes actions to
%% be performed.
-record(event,
        { name=error('event.name') :: atom()            %class identifier
        , id=''                    :: _                 %instance identifier
        , input=eon:new()          :: obj(_, _)         %payload
        , valid_from=0             :: non_neg_integer() %timestamp
        }).

%% A modron is an instance of a state machine.
-record(modron,
        { id=error('modron.id')       :: mechanus:id() %DB key
        , state=error('modron.state') :: #state{}      %current state
        , data=eon:new()              :: mechanus:data() %collected [in|out]puts
        , events=[]                   :: [#event{}]    %unprocessed events
        , ev_hist=[]                  :: [#event{}]    %processed events (rev)
        , actions=[]                  :: [module()]    %unprocessed actions
        , act_hist=[]                 :: [[module()]]    %processed actions (rev)
        , spec=error('modron.spec')   :: _             %FSM specification
        }).

%% Action callbacks must return these:
-record(result,
        { output=eon:new() :: obj(_, _)  %return values
        , events=[]        :: [#event{}] %feedback events
        }).

-type obj(A, B)  :: eon:object(A, B).
-type mid()      :: mechanus:id().
-type tid()      :: any(). % mechanus_queue:id().
-type filename() :: string() | atom().

%%%_* Footer ===========================================================
-endif. %include guard

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
