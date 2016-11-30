%% Copyright (c) 2007-2016 Pivotal Software, Inc.
%% You may use this code for any purpose.

-module(rabbit_extractor).

-behaviour(application).

-export([start/0, stop/0, start/2, stop/1]).

start() ->
    rabbit_extractor_sup:start_link(),
     ok.

start(normal, []) ->
    rabbit_extractor_sup:start_link().

stop() ->
    ok.

stop(_State) ->
    ok.
