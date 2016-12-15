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

-module(rabbit_extractor_worker).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([send_config/2, fetch_remote_status/1]).

-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

send_config(Url, Data) ->
    gen_server:call({global, ?MODULE}, {send_config, Url, Data}).

fetch_remote_status(Url) ->
    gen_server:call({global, ?MODULE}, {fetch_remote_status, Url}).

%---------------------------
% Gen Server Implementation
% --------------------------

init([]) ->
    rabbit_log:info("*> extractor worker gen_server initialized."),
    {ok, #state{}}.

handle_call({send_config, Url, Data}, _From, State) ->
    Result = send_data(Data, Url),
    {reply, Result, State};

handle_call({fetch_remote_status, Url}, _From, State) ->
    Result = fetch_remote_state(Url),
    {reply, Result, State};

handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%---------------------------

send_data(Data, Url) ->
    URL = binary_to_list(Url),
    Method = post,
    Header = [],
    Type = "application/json",
    HTTPOptions = [],
    Options = [],
    Result = httpc:request(Method, {URL, Header, Type, binary_to_list(Data)}, HTTPOptions, Options),
    check_result_state(Result).

fetch_remote_state(Url) ->
    URL = binary_to_list(Url),
    Method = head,
    HTTPOptions = [],
    Options = [],
    Result = httpc:request(Method, {URL, []}, HTTPOptions, Options),
    check_result_state(Result).

%--------------------------

check_result_state(Result) ->
    {State, Content} = Result,
    case State of
        ok -> {{"HTTP/1.1", ReturnCode, ReturnMsg}, _Head, _Body} = Content,
              if
                  ReturnCode >= 300 -> {error, {list_to_atom(ReturnMsg), ReturnCode}};
                  true ->
                      {ok, Content}
              end;
        _ -> Result
    end.
