%%%------------------------------------------------------------------------
%% Copyright 2017, OpenCensus Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Opencensus measures and views for Elli stats.
%% @end
%%%-------------------------------------------------------------------------
-module(oc_elli).

-include_lib("opencensus/include/opencensus.hrl").

-export([default_views/0,
         register_measures/0]).

register_measures() ->
    oc_stat_measure:new('opencensus.io/http/server/received_bytes', "Total bytes received in request body (not including headers). This is uncompressed bytes.", bytes),
    oc_stat_measure:new('opencensus.io/http/server/sent_bytes', "Total bytes sent in response bodies.", bytes),
    oc_stat_measure:new('opencensus.io/http/server/server_latency', "Time between first byte of request headers read to last byte of response sent, or terminal error.", milli_seconds).


default_views() ->
    [#{name => "opencensus.io/http/server/completed_count",
       description => "Count of HTTP requests completed",
       tags => [http_server_method, http_server_path, http_server_status],
       measure => 'opencensus.io/http/server/server_latency',
       aggregation => oc_stat_aggregation_count},
    #{name => "opencensus.io/http/server/received_bytes",
       description => "Size distribution of HTTP bytes received",
       tags => [http_server_method, http_server_path],
       measure => 'opencensus.io/http/server/received_bytes',
       aggregation => default_size_distribution()},
     #{name => "opencensus.io/http/server/sent_bytes",
       description => "Size distribution of HTTP bytes sent",
       tags => [http_server_method, http_server_path],
       measure => 'opencensus.io/http/server/sent_bytes',
       aggregation => default_size_distribution()},
     #{name => "opencensus.io/http/server/server_latency",
       description => "Latency distribution of HTTP requests",
       tags => [http_server_method, http_server_path],
       measure => 'opencensus.io/http/server/server_blatency',
       aggregation => default_latency_distribution()}].

default_size_distribution() ->
    {oc_stat_aggregation_distribution, [{buckets, [0, 1024, 2048, 4096, 16384, 65536,
                                                   262144, 1048576, 4194304, 16777216,
                                                   67108864, 268435456, 1073741824,
                                                   4294967296]}]}.

default_latency_distribution() ->
    {oc_stat_aggregation_distribution, [{buckets, [0, 1, 2, 3, 4, 5, 6, 8, 10, 13, 16, 20, 25, 30,
                                                   40, 50, 65, 80, 100, 130, 160, 200, 250, 300, 400,
                                                   500, 650, 800, 1000, 2000, 5000, 10000, 20000, 50000,
                                                   100000]}]}.
