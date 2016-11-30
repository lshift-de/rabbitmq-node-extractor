%% Copyright (c) 2007-2016 Pivotal Software, Inc.
%% You may use this code for any purpose.

-module(rabbit_extractor_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{rabbit_extractor_worker,
            {rabbit_extractor_worker, start_link, []},
            permanent,
            10000,
            worker,
            [rabbit_extractor_worker]}
          ]}}.
