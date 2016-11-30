%% Copyright (c) 2007-2016 Pivotal Software, Inc.
%% You may use this code for any purpose.

-module(rabbit_extractor_worker).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([send_config/2, fetch_remote_status/1]).

%%-include_lib("amqp_client/include/amqp_client.hrl").

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
