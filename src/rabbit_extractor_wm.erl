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

-module(rabbit_extractor_wm).

-export([init/3, rest_init/2, to_json/2, content_types_provided/2, is_authorized/2]).

-import(rabbit_misc, [pget/2]).

-include_lib("rabbitmq_management/include/rabbit_mgmt.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------

init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #context{}}.

content_types_provided(ReqData, Context) ->
    {[{<<"application/json">>, to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    rabbit_mgmt_util:reply([{vhosts, get_vhosts_for_user(ReqData, Context)},
                            {users, users()}], ReqData, Context).

is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized_admin(ReqData, Context).


%%-------------------------------------------------------------------

get_vhosts_for_user(_ReqData, #context{user = User}) ->
    rabbit_mgmt_util:list_login_vhosts(User, undefined).

users() ->
    [begin
         {ok, User} = rabbit_auth_backend_internal:lookup_user(pget(user, U)),
         [{Key, Value} || {Key, Value} <- rabbit_mgmt_format:internal_user (User),
                          lists:member(Key, ['name', 'tags'])]
     end || U <- rabbit_auth_backend_internal:list_users()].
