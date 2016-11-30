%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Initial Developer of the Original Code is GoPivotal, Inc.
%%   Copyright (c) 2011-2015 GoPivotal, Inc.  All rights reserved.

-module(rabbit_extractor_settings_wm).

-export([init/3, rest_init/2, to_json/2, content_types_provided/2, is_authorized/2]).
-export([content_types_accepted/2, allowed_methods/2, accept_json/2]).
-export([accept_multipart/2]).
-export([variances/2]).

-import(rabbit_misc, [pget/2]).
-import(rabbit_extractor_utils, [get_all_parts/2, get_part/2]).

-include_lib("rabbitmq_management/include/rabbit_mgmt.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------

init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Config) ->
    {ok, rabbit_mgmt_cors:set_headers(Req, ?MODULE), #context{}}.

variances(Req, Context) ->
    {[<<"accept-encoding">>, <<"origin">>], Req, Context}.

content_types_provided(ReqData, Context) ->
    {[{<<"application/json">>, to_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"OPTIONS">>], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{<<"application/json">>, accept_json},
      {{<<"multipart">>, <<"form-data">>, '*'}, accept_multipart}], ReqData, Context}.

to_json(ReqData, Context) ->
    vhost_definitions(ReqData, Context).

is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized_admin(ReqData, Context).


%%-------------------------------------------------------------------

vhost_definitions(ReqData, Context) ->
    Xs = [X || X <- exchanges_basic(ReqData),
               export_exchange(X)],
    Qs = [Q || Q <- queues_basic(ReqData),
               export_queue(Q)],
    QNames = [{pget(name, Q), pget(vhost, Q)} || Q <- Qs],
    Bs = [B || B <- bindings_basic(ReqData),
               export_binding(B, QNames)],
    {ok, Vsn} = application:get_key(rabbit, vsn),
    rabbit_mgmt_util:reply(
      [{rabbit_version, list_to_binary(Vsn)}] ++
      filter(
        [{users,       users_basic(ReqData)},
         {vhosts,      vhosts_basic(ReqData)},
         {permissions, permissions_basic(ReqData)},
         {parameters,  parameters_basic(ReqData)},
         {policies,    policies_basic(ReqData)},
         {queues,      Qs},
         {exchanges,   Xs},
         {bindings,    Bs}]),
      case cowboy_req:qs_val(<<"download">>, ReqData) of
          {undefined, _} -> ReqData;
          {Filename, _}  -> rabbit_mgmt_util:set_resp_header(
                         "Content-Disposition",
                         "attachment; filename=" ++
                             binary_to_list(Filename), ReqData)
      end,
      Context).

%%-------------------------------------------------------------------

all_or_one_vhost(ReqData, Fun) ->
    case rabbit_mgmt_util:id(vhost, ReqData) of
        none      -> rabbit_log:info("no vhost to export"),
                     [];
        not_found -> rabbit_log:info("export vhost not found"),
                     vhost_not_found;
        VHost     -> VhostList = string:tokens(binary_to_list(VHost), "|"),
                     lists:append([Fun(list_to_binary(V)) || V <- VhostList])
    end.

users_basic(ReqData) ->
    case rabbit_mgmt_util:id(user, ReqData) of
        none      -> rabbit_log:info("no user to export...", []), [];
        not_found -> rabbit_log:info("export user not found ...", []), [];
        Users     -> UserList = string:tokens(binary_to_list(Users), "|"),
                     [ User || User <- rabbit_mgmt_wm_users:users(),
                               lists:member(binary_to_list(pget(name, User)), UserList)]
    end.


exchanges_basic(ReqData) ->
    [rabbit_mgmt_format:exchange(X) || X <- all_or_one_vhost(ReqData, fun rabbit_exchange:info_all/1)].

queues_basic(ReqData) ->
    [rabbit_mgmt_format:queue(Q) || Q <- all_or_one_vhost(ReqData, fun rabbit_amqqueue:list/1)] ++
        [rabbit_mgmt_format:queue(Q#amqqueue{state = down}) ||
            Q <- all_or_one_vhost(ReqData, fun rabbit_amqqueue:list_down/1)].

bindings_basic(ReqData) ->
    [rabbit_mgmt_format:binding(B) ||
        B <- all_or_one_vhost (ReqData,
                               fun (VHost)    ->
                                       rabbit_binding:list(VHost)
                               end)].

vhosts_basic(ReqData) ->
    [[V] || V <- all_or_one_vhost(ReqData,
                                  fun (VHost) ->
                                          rabbit_vhost:info(VHost, [name])
                                  end)].


parameters_basic(ReqData) ->
    Raw = case rabbit_mgmt_util:id(component, ReqData) of
              none  -> rabbit_runtime_parameters:list();
              Name  -> case rabbit_mgmt_util:id(vhost, ReqData) of
                           none      -> rabbit_runtime_parameters:list_component(
                                          Name);
                           not_found -> not_found;
                           VHost     -> VhostList = string:tokens(binary:bin_to_list(VHost), "|"),
                                        lists:append([rabbit_runtime_parameters:list(V, Name) || V <- VhostList])
                       end
          end,
    case Raw of
        not_found -> not_found;
        _         -> [rabbit_mgmt_format:parameter(rabbit_mgmt_wm_parameters:fix_shovel_publish_properties(P)) || P <- Raw]
    end.

policies_basic(ReqData) ->
    case rabbit_mgmt_util:id(vhost, ReqData) of
        not_found -> not_found;
        none      -> rabbit_policy:list();
        VHost     -> VhostList = string:tokens(binary_to_list(VHost), "|"),
                     lists:append([rabbit_policy:list(list_to_binary(V)) || V <- VhostList])
    end.

permissions_basic(ReqData) ->
    case rabbit_mgmt_util:id(vhost, ReqData) of
        not_found -> not_found;
        none      -> [];
        VHost     -> VhostList = string:tokens(binary_to_list(VHost), "|"),
                     UserList = [ pget(name, User) || User <- users_basic(ReqData)],
                     AllPermissions = rabbit_mgmt_wm_permissions:permissions(),
                     [Permission || Permission <- AllPermissions,
                                    lists:member(binary_to_list(pget(vhost, Permission)), VhostList),
                                    lists:member(pget(user,Permission), UserList)]
    end.

%%-------------------------------------------------------------------

rw_state() ->
    [{users,       [name, password_hash, hashing_algorithm, tags]},
     {vhosts,      [name]},
     {permissions, [user, vhost, configure, write, read]},
     {parameters,  [vhost, component, name, value]},
     {policies,    [vhost, name, pattern, definition, priority, 'apply-to']},
     {queues,      [name, vhost, durable, auto_delete, arguments]},
     {exchanges,   [name, vhost, type, durable, auto_delete, internal,
                    arguments]},
     {bindings,    [source, vhost, destination, destination_type, routing_key,
                    arguments]}].

filter(Items) ->
    [filter_items(N, V, proplists:get_value(N, rw_state())) || {N, V} <- Items].

filter_items(Name, List, Allowed) ->
    {Name, [filter_item(I, Allowed) || I <- List]}.

filter_item(Item, Allowed) ->
    [{K, Fact} || {K, Fact} <- Item, lists:member(K, Allowed)].

export_exchange(Exchange) ->
    export_name(pget(name, Exchange)).

export_name(<<>>)                 -> false;
export_name(<<"amq.", _/binary>>) -> false;
export_name(_Name)                -> true.

export_queue(Queue) ->
    pget(owner_pid, Queue) == none.

export_binding(Binding, Qs) ->
    Src      = pget(source,           Binding),
    Dest     = pget(destination,      Binding),
    DestType = pget(destination_type, Binding),
    VHost    = pget(vhost,            Binding),
    Src =/= <<"">>
        andalso
          ( (DestType =:= queue andalso lists:member({Dest, VHost}, Qs))
            orelse (DestType =:= exchange andalso Dest =/= <<"">>) ).

%%-------------------------------------------------------------------

accept_json(ReqData0, Context) ->
    {ok, _Body, ReqData} = cowboy_req:body(ReqData0),
    {true, ReqData, Context}.

accept_multipart(ReqData0, Context) ->
    {Parts, ReqData} = get_all_parts(ReqData0, []),
    Json = get_part(<<"current-config">>, Parts),
    TargetUrl = get_part(<<"target-url">>, Parts),
    VhostsSelected = get_part(<<"selected-hosts">>, Parts),
    {ReturnState, ResponseBody} = rabbit_extractor_worker:send_config(TargetUrl, Json),

    createShovels(VhostsSelected, Context),

    case ReturnState of
        ok -> ReqData2 = cowboy_req:set_resp_body(<<"ok">>, ReqData),
              {true, ReqData2, Context};
        _ -> Reason = if is_tuple(ResponseBody) -> [Reason1 | _Rest] = ResponseBody,
                                                   Reason1;
                         true -> ResponseBody
                      end,
             Resp = cowboy_req:set_resp_body(list_to_binary(atom_to_list(Reason)), ReqData),
             {ok, Resp2} = cowboy_req:reply(404, Resp),
             {halt, Resp2, Context}
    end.

%%-------------------------------------------------------------------

createShovels(Vhosts, _Context = #context{user = User}) ->

    VhostList = string:tokens(binary_to_list(Vhosts), "|"),
    Queues = lists:append([rabbit_amqqueue:list(list_to_binary(Q1)) || Q1 <- VhostList]),
    DownQueues = lists:append([rabbit_amqqueue:list_down(list_to_binary(Q2)) || Q2 <- VhostList]),
    AllQueues = [rabbit_mgmt_format:queue(Q) || Q <- Queues] ++
        [rabbit_mgmt_format:queue(Q) || Q <- DownQueues],

    ShovelQueues = [begin
                        QName = pget(name, Q),
                        VHost = pget(vhost, Q),
                        %#user {username = Username} = User,
                        SrcUri = list_to_binary(
                                   io_lib:format(
                                     "amqp://~s:~s/~s",
                                     ["localhost", "5672", binary_to_list(VHost)])),

                        Params = [{"src-uri", SrcUri},
                                  {"src-queue", QName},
                                  {"dest-uri", <<"amqp://first-user:test@localhost:5673/first.host">>},
                                  {"dest-queue", QName},
                                  {"delete-after", <<"queue-length">>},
                                  {"ack-mode", <<"on-publish">>}],

                        ParamsConverted = [ {list_to_binary(X),Y} || {X,Y} <- Params ],

                        rabbit_log:info("*> shovel parameters before setting them... - ~p~n", [ParamsConverted]),

                        case rabbit_runtime_parameters:set(
                               VHost, <<"shovel">>, "extractorshovel-" ++ binary_to_list(QName),
                               if
                                   is_map(ParamsConverted) -> maps:to_list(ParamsConverted);
                                   true -> ParamsConverted
                               end,
                               User) of
                            ok ->
                                {true, <<"Ok">>};
                            {_, Reason} ->
                                {error, Reason}
                        end
                    end || Q <- AllQueues],

    rabbit_log:info("*> shovel parameters... - ~p~n", [ShovelQueues]).
