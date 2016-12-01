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

-module(rabbit_extractor_dispatch).

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0, web_ui/0]).

dispatcher() -> [{"/node_extractor", rabbit_extractor_wm, []},
                 {"/remote_status", rabbit_extractor_rstate_wm, []},
                 {"/extract_settings", rabbit_extractor_settings_wm, []},
                 {"/extract_settings/:vhost", rabbit_extractor_settings_wm, []},
                 {"/extract_settings/:vhost/:user", rabbit_extractor_settings_wm, []}].

web_ui()     -> [{javascript, <<"node_extractor.js">>}].
