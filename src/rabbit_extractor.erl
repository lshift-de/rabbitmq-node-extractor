%%
%%   This file is part of rabbitmq node extractor.
%%   Copyright © 2016 LShift Services GmbH
%%
%%   rabbitmq node extractor is free software: you can redistribute it and/or
%%   modify it under the terms of the GNU Lesser General Public License as
%%   published by the Free Software Foundation, either version 3 of the License,
%%   or (at your option) any later version.
%%
%%   rabbitmq node extractor is distributed in the hope that it will be useful,
%%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%   GNU Lesser General Public License for more details.
%%
%%   You should have received a copy of the GNU Lesser General Public License
%%   along with rabbitmq node extractor. If not, see <http://www.gnu.org/licenses/>.

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
