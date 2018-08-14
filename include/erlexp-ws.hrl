-type host()            :: list().
-type port()            :: pos_integer().

-type start_options()   :: erlexp:start_options() | #{
    'cserver_host'                  => host(),
    'cserver_port'                  => port()
}.

-record('transport_state', {
        'self_pid'                              :: pid(),
        'cserver_host' = "localhost"            :: host(),
        'cserver_port' = 443                    :: port()
    }).

-type transport_state()                :: #transport_state{}.
