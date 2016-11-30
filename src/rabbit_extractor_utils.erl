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
%%   Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_extractor_utils).

-export([get_all_parts/2, get_part/2]).


get_all_parts(ReqData0, Acc) ->
    case cowboy_req:part(ReqData0) of
        {done, ReqData} ->
            {Acc, ReqData};
        {ok, Headers, ReqData1} ->
            Name = case cow_multipart:form_data(Headers) of
                       {data, N} -> N;
                       {file, N, _, _, _} -> N
                   end,
            {ok, Body, ReqData} = cowboy_req:part_body(ReqData1),
            get_all_parts(ReqData, [{Name, Body}|Acc])
    end.

get_part(Name, Parts) ->
    case lists:keyfind(Name, 1, Parts) of
        false -> unknown;
        {_, Value} -> Value
    end.
