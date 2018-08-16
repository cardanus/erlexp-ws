%% --------------------------------------------------------------------------------
%% File:    erlexp-ws.erl
%% @author  Oleksii Semilietov <spylik@gmail.com>
%% --------------------------------------------------------------------------------
-module('erlexp-ws').

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-define(SERVER, ?MODULE).

-include("erlexp-ws.hrl").
-include("deps/erlexp/include/logger_macroses.hrl").

-behaviour(gen_server).

% @doc export gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% @doc export public api
-export([
        start/1,
        stop/1,
        stop/2
    ]).

-export_type([start_options/0]).

% @doc start api
-spec start(Options) -> Result when
    Options     :: start_options(),
    Result      :: {ok, Pid} | 'ignore' | {'error', Error},
    Pid         :: pid(),
    Error       :: {already_started, Pid} | term().

start(Options) ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, Options, []).

% @doc API for stop gen_server. Default is sync call.
-spec stop(Server) -> Result when
    Server      :: server(),
    Result      :: term().

stop(Server) ->
    stop('sync', Server).

% @doc API for stop gen_server. We support async casts and sync calls aswell.
-spec stop(SyncAsync, Server) -> Result when
    SyncAsync   :: 'sync' | 'async',
    Server      :: server(),
    Result      :: term().

stop('sync', Server) ->
    gen_server:stop(Server);
stop('async', Server) ->
    gen_server:cast(Server, stop).

% ============================ gen_server part =================================

-spec init(Options) -> Result when
    Options :: start_options(),
    Result  :: {'ok', transport_state()}.

init(Options) ->
    Heartbeat_freq = maps:get('heartbeat_freq', Options,
        application:get_env(?MODULE, 'heartbeat_freq', 1000)
    ),

    Timeout_before_ping = maps:get('timeout_before_ping', Options,
        application:get_env(?MODULE, 'timeout_before_ping', 'from_remote')
    ),

    {TimeoutLastGlobalFrame, SetTimeoutBeforePing} = case Timeout_before_ping of
        'from_remote' ->
            {'undefined', 'undefined'};
        _ ->
            {Timeout_before_ping + Timeout_for_ping, Timeout_before_ping}
    end,

    State = may_need_connect(#transport_state{
        'heartbeat_freq' = Heartbeat_freq,
        'timeout_for_gun_ws_upgrade' = maps:get('timeout_for_gun_ws_upgrade', Options,
            application:get_env(?MODULE, 'timeout_for_gun_ws_upgrade', 10000)
        ),
        'timeout_for_subscribtion' = maps:get('timeout_for_subscribtion', Options,
            application:get_env(?MODULE, 'timeout_for_subscribtion', 10000)
        ),
        'timeout_before_ping' = Timeout_before_ping,
        'timeout_for_ping' = maps:get('timeout_for_ping', Options,
            application:get_env(?MODULE, 'timeout_for_ping' 30000)
        ),
        'timeout_last_global_frame' = TimeoutLastGlobalFrame,
        'timeout_before_ping_set' = SetTimeoutBeforePing
    }),

    {'ok',
        State#transport_state{
            'heartbeat_tref' = erlang:send_after(Heartbeat_freq, self(), 'heartbeat')
        }
    }.

%--------------handle_call-----------------

% @doc handle_call for all other thigs
-spec handle_call(Message, From, State) -> Result when
    Message :: term(),
    From    :: {pid(), Tag},
    Tag     :: term(),
    State   :: transport_state(),
    Result  :: {reply, 'ok', State}.

handle_call(Msg, _From, State) ->
    ?warning("we are in undefined handle_call with message ~p\n",[Msg]),
    {reply, 'ok', State}.
%-----------end of handle_call-------------

%--------------handle_cast-----------------

% @doc handle cast for subscribe

-spec handle_cast(Message, State) -> Result when
    Message         :: SubscribeMsg | 'ping',
    SubscribeMsg    :: {'subscribe', binary()},
    State           :: transport_state(),
    Result          :: {noreply, State} | {stop, normal, State}.

handle_cast({'subscribe', Channel}, State) ->
    {noreply, subscribe(State, Channel)};

% @doc send ping to remote
handle_cast('ping', State) ->
    NewState = may_need_connect(State),
    send(NewState, ?PingFrame),
    {noreply, NewState};

% handle_cast for all other thigs
handle_cast(Msg, State) ->
    ?warning("we are in undefined handle_cast with message ~p\n",[Msg]),
    {noreply, State}.
%-----------end of handle_cast-------------


%--------------handle_info-----------------

% @doc callbacks for gen_server handle_info.
-spec handle_info(Message, State) -> Result when
    Message :: 'heartbeat' | gun_ws() | gun_error() | down(),
    State   :: transport_state(),
    Result  :: {noreply, State}.


% @doc hearbeat for find and recovery dead connection
handle_info('heartbeat', State = #transport_state{
        heartbeat_tref = Heartbeat_tref,
        heartbeat_freq = Heartbeat_freq
    }) ->
    _ = erlang:cancel_timer(Heartbeat_tref),
    NewState = may_need_connect(State),
    may_need_send_ping(NewState),
    TRef = erlang:send_after(Heartbeat_freq, self(), heartbeat),
    {noreply, NewState#transport_state{heartbeat_tref=TRef}};

% @doc connection established when timeout_before_ping = 'from_pusher'
handle_info({'gun_ws', _ConnPid, {text, <<"{\"event\":\"pusher:connection_established\"", _Rest/binary>> = Frame}}, State = #transport_state{timeout_before_ping = 'from_pusher', timeout_for_ping = Timeout_for_ping}) ->
    NewState = subscribe(State, 'all'),
    [_, TimeoutHere] = binary:split(Frame, <<"\\\"activity_timeout\\\":">>),
    [TimeOut] = binary:split(TimeoutHere, <<"}\"}">>, [trim]),
    TimeoutMS = erlang:convert_time_unit(binary_to_integer(TimeOut), seconds, milli_seconds),
    {noreply, NewState#transport_state{
            last_frame = get_time(),
            timeout_before_ping_set = TimeoutMS,
            timeout_last_global_frame = TimeoutMS+Timeout_for_ping
        }
    };

% @doc connection established when have timeout_before_ping
handle_info({'gun_ws', _ConnPid, {text, <<"{\"event\":\"pusher:connection_established\"", _Rest/binary>>}}, State) ->
    NewState = subscribe(State, 'all'),
    {noreply, NewState#transport_state{
            last_frame=get_time()
        }
    };

% @doc subscribed to channel
handle_info({'gun_ws', _ConnPid, {text, <<"{\"event\":\"pusher_internal:subscription_succeeded\",\"data\":\"{}\",\"channel\":\"", ChannelHere/binary>>}}, State = #transport_state{channels = Channels}) ->
    Time = get_time(),
    [Channel] = binary:split(ChannelHere, <<"\"}">>, [trim]),
    ChannelData = maps:get(Channel, Channels),
    {noreply, State#transport_state{
            last_frame = Time,
            channels = Channels#{Channel := ChannelData#channel_prop{status = 'connected', get_sub_confirm = Time, last_frame = Time}}
        }
    };

handle_info({'gun_ws', _ConnPid, {text, <<"{\"event\":\"pusher:pong\",\"data\":\"{}\"}">>}}, State) ->
    {noreply, State#transport_state{last_frame = get_time()}};

%% ---- close and other events bringing gun to flush ---- %%

% @doc gun_ws close
handle_info({'gun_ws', ConnPid, 'close'}, State) ->
    gun:ws_send(ConnPid, 'close'),
    {noreply, flush_gun(State, ConnPid)};

% @doc gun_ws close with code
handle_info({'gun_ws', ConnPid, {'close', Code, _}}, State) ->
    gun:ws_send(ConnPid, {'close', Code, <<>>}),
    {noreply, flush_gun(State, ConnPid)};

% @doc gun_error
handle_info({'gun_error', ConnPid, Reason}, State) ->
    ?error("got gun_error for ConnPid ~p with reason: ~p",[ConnPid, Reason]),
    {noreply, flush_gun(State, ConnPid)};

% @doc gun_down
handle_info({'gun_down', ConnPid, 'ws', _, _, _}, State) ->
    {noreply, flush_gun(State, ConnPid)};

% @doc expected down with state 'normal'
handle_info({'DOWN', _MonRef, 'process', ConnPid, 'normal'}, State) ->
    {noreply, flush_gun(State, ConnPid)};

% @doc unexepted 'DOWN'
handle_info({'DOWN', MonRef, 'process', ConnPid, Reason}, State) ->
    ?info("got DOWN for ConnPid ~p, MonRef ~p with Reason: ~p",[ConnPid, MonRef, Reason]),
    {noreply, flush_gun(State, ConnPid)};

% @doc unknown gun_ws frame
handle_info({'gun_ws', _ConnPid, Frame}, State) ->
    ?warning("got non-text gun_ws event with Frame ~p", [Frame]),
    {noreply, State};

% @doc handle_info for unknown things
handle_info(Msg, State) ->
    ?warning("we are in undefined handle_info with message ~p\n",[Msg]),
    {noreply, State}.
%-----------end of handle_info-------------

% @doc call back for terminate (we going to cancel timer here)
-spec terminate(Reason, State) -> term() when
    Reason :: 'normal' | 'shutdown' | {'shutdown',term()} | term(),
    State :: term().

terminate(_Reason, #transport_state{gun_pid = GunPid} = State) ->
    flush_gun(State, GunPid).

% @doc call back for code_change
-spec code_change(OldVsn, State, Extra) -> Result when
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term(),
    State :: term(),
    Extra :: term(),
    Result :: {ok, NewState},
    NewState :: term().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% --------------------------- end of gen_server part ---------------------------

% =========================== other private functions ==========================

% @doc check does we need to connect or reconnect
-spec may_need_connect(State) -> Result when
    State   :: transport_state(),
    Result  :: transport_state().

may_need_connect(State = #transport_state{gun_pid = 'undefined'}) ->
    connect(State);
may_need_connect(State = #transport_state{
        last_frame = LastFrame,
        gun_pid = GunPid,
        timeout_last_global_frame = Timeout_last_global_frame
        }) when is_integer(LastFrame), is_integer(Timeout_last_global_frame) ->
    case get_time() - LastFrame > Timeout_last_global_frame of
        true ->
            connect(flush_gun(State, GunPid));
        false ->
            State
    end;
may_need_connect(State) ->
    State.

-spec may_need_send_ping(State) -> boolean() when
    State :: transport_state().

may_need_send_ping(#transport_state{gun_pid = 'undefined'}) -> false;
may_need_send_ping(#transport_state{last_frame = 'undefined'}) -> false;
may_need_send_ping(#transport_state{
        last_frame = LastFrame,
        connected_since = Connected_since,
        timeout_before_ping_set = Timeout_before_ping_set} = State) ->
    Time = get_time(),
    Online = Time - Connected_since,
    Expired = Time - LastFrame,
    case Online > Timeout_before_ping_set of
        true when Expired > Timeout_before_ping_set ->
            send(State, ?PingFrame),
            true;
        _ ->
            false
    end.

% @doc connect
-spec connect(State) -> Result when
    State   :: transport_state(),
    Result  :: transport_state().

connect(State = #transport_state{
        pusher_url = Pusher_url,
        timeout_for_gun_ws_upgrade = Timeout_for_gun_ws_upgrade}) ->
    ?info("Trying connect with state ~p", [State]),
    {ok, Pid} = gun:open("wss.pusherapp.com", 443, #{retry=>0}),
    GunMonRef = monitor(process, Pid),
    case gun:await_up(Pid, 5000, GunMonRef) of
        {ok, Protocol} ->
            ?info("Gun ~p connected to remote with protocol ~p",[Pid, Protocol]),
            gun:ws_upgrade(Pid, Pusher_url, [], #{compress => true}),
            receive
                {'gun_ws_upgrade', Pid, ok, _} ->
                    ?info("Gun ~p got gun_ws_upgrade",[Pid]),
                    State#transport_state{gun_pid = Pid, gun_mon_ref = GunMonRef, connected_since = get_time()}
            after Timeout_for_gun_ws_upgrade ->
                ?info("Gun ~p got timeout_for_gun_ws_upgrade",[Pid]),
                flush_gun(State, Pid)
            end;
        {error, Reason} ->
            ?warning("Some error in gun ~p occur in gun during connection to pusher.com host",[Pid, Reason]),
            flush_gun(State, Pid)
    end.

% @doc subscribe section
-spec subscribe(State, Channel) -> Result when
    State   :: transport_state(),
    Channel :: 'all' | binary(),
    Result  :: transport_state().

subscribe(State = #transport_state{channels = Channels, timeout_for_subscribtion = Timeout_for_subscribtion}, 'all') ->
    Time = get_time(),
    NewState = may_need_connect(State),
    NewState#transport_state{channels = maps:map(
        fun
            % cast connect for channels with status 'to_connect'
            (Channel, #channel_prop{status = 'to_connect'} = ChannelProp) ->
            case send(NewState, {text, <<"{\"event\": \"pusher:subscribe\", \"data\": {\"channel\": \"", Channel/binary, "\"} }">>}) of
                true -> ChannelProp#channel_prop{status = 'casted', cast_sub = Time};
                false -> ChannelProp
            end;
            % cast connect for channels which do not get subscribtion confirmation
            (Channel, #channel_prop{status = 'casted', cast_sub = CastSub} = ChannelProp) when CastSub+Timeout_for_subscribtion < Time ->
            case send(NewState, {text, <<"{\"event\": \"pusher:subscribe\", \"data\": {\"channel\": \"", Channel/binary, "\"} }">>}) of
                true -> ChannelProp#channel_prop{status = 'casted', cast_sub = Time};
                false -> ChannelProp
            end;
            % for rest, do nothing
            (_Channel, ChannelProp) -> ChannelProp
        end, Channels
    )};

subscribe(State = #transport_state{channels = Channels}, Channel) when is_binary(Channel) ->
    Time = get_time(),
    NewRootState = case maps:is_key(Channel, Channels) of
        false ->
            NewState = may_need_connect(State),
            case send(NewState, {text, <<"{\"event\": \"pusher:subscribe\", \"data\": {\"channel\": \"", Channel/binary, "\"} }">>}) of
                true ->
                    NewState#transport_state{channels = Channels#{Channel => #channel_prop{status = 'casted', created = Time, cast_sub = Time}}};
                false ->
                    NewState#transport_state{channels = Channels#{Channel => #channel_prop{status = 'to_connect', created = Time}}}
            end;
        true ->
            State
    end,
    NewRootState.

% @doc send frame to remote
-spec send(State, Frame) -> Result when
    State :: transport_state(),
    Frame :: {'text', binary()},
    Result :: boolean().

send(#transport_state{gun_pid = 'undefined'}, _Frame) ->
    false;
send(#transport_state{gun_pid = GunPid}, Frame) ->
    gun:ws_send(GunPid, Frame),
    true.

% @doc gun clean_up
-spec flush_gun(State, GunPid) -> Result when
    State   :: transport_state(),
    GunPid  :: 'undefined' | pid(),
    Result  :: transport_state().

flush_gun(State = #transport_state{gun_mon_ref = Gun_mon_ref, gun_pid = Gun_pid}, ConnRef) ->
    case ConnRef =:= 'undefined' of
        true when Gun_pid =/= 'undefined' ->
            demonitor(Gun_mon_ref, [flush]),
            gun:close(Gun_pid),
            flush_state(State);
        true ->
            State;
        false when Gun_pid =:= 'undefined' ->
            gun:close(ConnRef),
            State;
        false when Gun_pid =:= ConnRef ->
            demonitor(Gun_mon_ref, [flush]),
            gun:close(ConnRef),
            flush_state(State);
        false when Gun_pid =/= ConnRef ->
            gun:close(ConnRef),
            State
    end.

% @doc flush transport state
-spec flush_state(State) -> Result when
    State   :: transport_state(),
    Result  :: transport_state().

flush_state(State = #transport_state{channels = Channels, timeout_before_ping = Timeout_before_ping}) ->
    NewState = State#transport_state{
        last_frame = 'undefined',
        gun_pid = 'undefined',
        gun_mon_ref = 'undefined',
        connected_since = 'undefined',
        channels = maps:map(
            fun
                % change state for channels with status 'casted' and 'connected'.
                (_Channel, #channel_prop{status = 'casted'} = ChannelProp) ->
                    ChannelProp#channel_prop{status = 'to_connect', cast_sub = 'undefined', get_sub_confirm = 'undefined'};
                (_Channel, #channel_prop{status = 'connected'} = ChannelProp) ->
                    ChannelProp#channel_prop{status = 'to_connect', cast_sub = 'undefined', get_sub_confirm = 'undefined'};
                % for rest, do nothing
                (_Channel, ChannelProp) ->
                    ChannelProp
            end, Channels
    )},
    case Timeout_before_ping of
        'from_pusher' ->
            NewState#transport_state{
                timeout_last_global_frame = 'undefined',
                timeout_before_ping_set = 'undefined'
            };
        _ ->
            NewState
    end.

% @doc get time
-spec get_time() -> Result when
    Result :: 'milli_seconds' | pos_integer().
get_time() ->
    erlang:system_time(milli_seconds).

% ------------------------ end of other private functions ----------------------

