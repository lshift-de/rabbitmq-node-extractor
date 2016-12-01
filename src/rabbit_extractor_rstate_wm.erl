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

-module(rabbit_extractor_rstate_wm).

-export([init/3, rest_init/2, is_authorized/2]).
-export([content_types_accepted/2, allowed_methods/2]).
-export([accept_multipart/2]).
-export([variances/2]).

-import(rabbit_extractor_utils, [get_all_parts/2, get_part/2]).

-include_lib("rabbitmq_management/include/rabbit_mgmt.hrl").

%%--------------------------------------------------------------------

init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Config) ->
    {ok, rabbit_mgmt_cors:set_headers(Req, ?MODULE), #context{}}.

variances(Req, Context) ->
    {[<<"accept-encoding">>, <<"origin">>], Req, Context}.

allowed_methods(ReqData, Context) ->
    {[<<"POST">>], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{{<<"multipart">>, <<"form-data">>, '*'}, accept_multipart}], ReqData, Context}.

is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized_admin(ReqData, Context).


%%-------------------------------------------------------------------

accept_multipart(ReqData0, Context) ->
    {Parts, ReqData} = get_all_parts(ReqData0, []),
    TargetUrl = get_part(<<"remote-url">>, Parts),
    {ReturnState, ResponseBody} = rabbit_extractor_worker:fetch_remote_status(TargetUrl),

    case ReturnState of
        ok -> ReqData2 = cowboy_req:set_resp_body(TargetUrl, ReqData),
              {true, ReqData2, Context};
        error ->  {Reason, _Rest} = ResponseBody,
                  Resp = cowboy_req:set_resp_body(list_to_binary(atom_to_list(Reason)), ReqData),
                  {ok, Resp2} = cowboy_req:reply(404, Resp),
                  {halt, Resp2, Context};
        _ -> Resp = cowboy_req:set_resp_body(<<"unknown state">>, ReqData),
             {ok, Resp2} = cowboy_req:reply(404, Resp),
             {halt, Resp2, Context}
    end.
