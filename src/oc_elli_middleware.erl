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
-include_lib("opencensus/include/opencensus.hrl").

-export([preprocess/2,
         handle/2,
         handle_event/3]).

preprocess(Req=#req{raw_path=RawPath, method=Method}, _) ->
    SpanCtxHeader = elli_request:get_header(oc_span_ctx_header:field_name(), Req, undefined),
    ParentSpanCtx = oc_span_ctx_header:decode(SpanCtxHeader),
    Operation = RawPath,
    SpanCtx = oc_trace:start_span(Operation, ParentSpanCtx,
                                  #{report_parent => true,
                                    kind => ?SPAN_KIND_SERVER,
                                    attributes => #{<<"http.url">> => RawPath,
                                                    <<"http.host">> =>
                                                        elli_request:get_header(<<"Host">>, Req, <<>>),
                                                    <<"http.user_agent">> =>
                                                        elli_request:get_header(<<"User-Agent">>, Req, <<>>),
                                                    <<"http.method">> =>
                                                        to_binary(Method)}}),
    ocp:with_span_ctx(SpanCtx),
    Req.

handle(_Req, _Config) ->
    ignore.

handle_event(request_complete, Args, Config) ->
    handle_full_response(request_complete, Args, Config);
handle_event(chunk_complete, Args, Config) ->
    handle_full_response(chunk_complete, Args, Config);

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

handle_full_response(_Type, [_Req, Code, _Hs, _B, {_Timings, Sizes}], _Config) ->
    UncompressedSize = size(Sizes, response_body),
    ocp:add_time_event(oc_trace:message_event(?MESSAGE_EVENT_TYPE_SENT, 0, UncompressedSize, UncompressedSize)),
    ocp:put_attribute(<<"http.status_code">>, Code),
    ocp:set_status(Code, undefined),
    ocp:finish_span().

size(Sizes, response_body) ->
    case proplists:get_value(chunks, Sizes) of
        undefined ->
            case proplists:get_value(file, Sizes) of
                undefined ->
                    proplists:get_value(resp_body, Sizes);
                FileSize -> FileSize
            end;
        ChunksSize -> ChunksSize
    end.

finish_exception(Exception, Stacktrace, _) ->
    ocp:put_attributes(#{<<"http.status_code">> => 500,
                         <<"stacktrace">> => term_to_string(Stacktrace),
                         <<"error.message">> => term_to_string(Exception)}),
    ocp:finish_span().

to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
to_binary(Method) ->
    Method.

term_to_string(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).
