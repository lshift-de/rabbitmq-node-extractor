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
