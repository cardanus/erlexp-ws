-type host()            :: list().
-type path()            :: list().
-type port()            :: pos_integer().

-type start_options()   :: erlexp:start_options() | #{
    'host'                          => host(),
    'port'                          => port(),
    'heartbeat_freq'                => pos_integer(),
    'timeout_for_gun_ws_upgrade'    => pos_integer(),
    'timeout_for_subscribtion'      => pos_integer(),
    'timeout_before_ping'           => 'from_remote' | pos_integer(),
    'timeout_for_ping'              => pos_integer()
}.

-type channel_status()          :: 'to_connect' | 'casted' | 'connected' | 'error'.
-type channel()                 :: binary().

-record(channel_prop, {
        status = 'to_connect'   :: channel_status(),
        created                 :: pos_integer(),
        cast_sub                :: 'undefined' | pos_integer(),
        get_sub_confirm         :: 'undefined' | pos_integer(),
        last_frame              :: 'undefined' | pos_integer()
    }).
-type channel_prop()            :: #channel_prop{}.

-type channels()                :: #{} | #{channel() => channel_prop()}.

-record('transport_state', {
         self_pid                               :: pid(),
         host = "localhost"                     :: host(),
         path = "/"                             :: path(),
         port = 443                             :: port(),
         heartbeat_freq = 1000                  :: pos_integer(),
         timeout_for_gun_ws_upgrade = 10000     :: pos_integer(),
         timeout_for_subscribtion = 10000       :: pos_integer(),
         timeout_before_ping = 'from_remote'    :: 'from_remote' | pos_integer(),   % timeout before we send ping message
         timeout_for_ping                       :: pos_integer(),                   % timeout for get ping message
         % changable section during state evolution
         channels                               :: channels(),                      % channels list
         timeout_before_ping_set                :: 'undefined' | pos_integer(),
         timeout_last_global_frame              :: 'undefined' | pos_integer(),     % timeout before we going to flush gun connection if haven't new messages
         gun_pid                                :: 'undefined' | pid(),             % current gun connection Pid
         gun_mon_ref                            :: 'undefined' | reference(),       % current gun monitor refference
         connected_since                        :: 'undefined' | pos_integer(),     % connected since
         last_frame                             :: 'undefined' | pos_integer(),     % last frame timestamp
         heartbeat_tref                         :: 'undefined' | reference()        % last heartbeat time refference
    }).

-type transport_state()                :: #transport_state{}.


