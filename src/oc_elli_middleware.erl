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
%% @doc Elli middleware for tracing requests
%% @end
%%%-------------------------------------------------------------------------
-module(oc_elli_middleware).

-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").

-export([preprocess/2,
         handle/2,
         handle_event/3,
         postprocess/3]).

-define(status(Res), case element(1, Res) of ok -> 200; S1 -> S1 end).

preprocess(Req=#req{raw_path=RawPath, method=Method}, _Config) ->
    case elli_request:get_header(<<"TraceContext">>, Req, undefined) of
        undefined ->
            ocp:start_trace();
        TCHeader ->
            ocp:start_trace(oc_trace_context_headers:decode(TCHeader))
    end,

    Operation = <<(to_binary(Method))/binary, ":", RawPath/binary>>,
    ocp:start_span(Operation),
    ocp:put_attributes(attributes(Req)),
    Req.

postprocess(_Req, Res, _Config) ->
    S = ?status(Res),
    ocp:put_attribute(<<"/http/status_code">>, integer_to_binary(S)),
    ocp:finish_span(),
    Res.

handle(_Req, _Config) ->
    ignore.

handle_event(request_timeout, _, Config) ->
    finish_exception(request_timeout, request_timeout, Config);
handle_event(request_parse_error, [Reason], Config) ->
    finish_exception(request_parse_error, Reason, Config);
handle_event(client_closed, [RequestPart], Config) ->
    finish_exception(client_closed, {request_part, RequestPart}, Config);
handle_event(client_timeout, [RequestPart], Config) ->
    finish_exception(client_timeout, {request_part, RequestPart}, Config);
handle_event(bad_request, [Reason], Config) ->
    finish_exception(bad_request, Reason, Config);
handle_event(request_error, [_Req, Exception, Stacktrace], Config) ->
    finish_exception(Exception, Stacktrace, Config);
handle_event(request_throw, [_Req, Exception, Stacktrace], Config) ->
    finish_exception(Exception, Stacktrace, Config);
handle_event(request_exit, [_Req, Exception, Stacktrace], Config) ->
    finish_exception(Exception, Stacktrace, Config);
handle_event(_Event, _Args, _Config) ->
    ok.

%%

finish_exception(Exception, Stacktrace, _) ->
    ocp:put_attribute(<<"/stacktrace">>, term_to_string(Stacktrace)),
    ocp:put_attribute(<<"/error/message">>, term_to_string(Exception)),
    ocp:finish_span().

attributes(#req{raw_path=Path,
                method=Method,
                headers=Headers}) ->
    maps:from_list([{<<"/span/kind">>, <<"server">>},
                    {<<"/http/url">>, Path},
                    {<<"/http/method">>, to_binary(Method)} | Headers]).


to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
to_binary(Method) ->
    Method.

term_to_string(Term) ->
    io_lib:format("~p", [Term]).
