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
