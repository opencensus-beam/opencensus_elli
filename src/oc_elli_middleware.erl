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
%% @doc Elli middleware for tracing requests and recording stats.
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

    %% update tags from header and new request tags
    BinMethod = to_binary(Method),
    UserAgent = elli_request:get_header(<<"User-Agent">>, Req, <<>>),
    Host = elli_request:get_header(<<"Host">>, Req, <<>>),
    TagMap = #{http_server_method => BinMethod,
               http_server_path => RawPath,
               http_server_host => Host},
    TagsHeader = elli_request:get_header(oc_tag_ctx_header:field_name(), Req, <<>>),
    case oc_tag_ctx_header:decode(TagsHeader) of
        {ok, ReqTags} ->
            ocp:with_tags(oc_tags:update(ReqTags, TagMap));
        {error, {_Module, _Error}} ->
            %% ?LOG_INFO(Module:format_error(Error))
            ocp:with_tags(oc_tags:new(TagMap))
    end,

    %% start child
    _ =  ocp:with_span_ctx(oc_trace:start_span(RawPath, ParentSpanCtx,
                                               #{remote_parent => true,
                                                 kind => ?SPAN_KIND_SERVER,
                                                 attributes => #{<<"http.url">> => RawPath,
                                                                 <<"http.host">> => Host,
                                                                 <<"http.user_agent">> => UserAgent,
                                                                 <<"http.method">> => BinMethod}})),
    Req.

handle(_Req, _Config) ->
    ignore.

handle_event(elli_startup, _Args, _Config) ->
    %% register measures, if not already registered
    oc_elli:register_measures(),
    ok;
handle_event(request_complete, Args, Config) ->
    handle_full_response(request_complete, Args, Config);
handle_event(chunk_complete, Args, Config) ->
    handle_full_response(chunk_complete, Args, Config);

handle_event(request_timeout, _, _Config) ->
    finish_exception(request_timeout, request_timeout);
handle_event(request_parse_error, [Reason], _Config) ->
    finish_exception(request_parse_error, Reason);
handle_event(client_closed, [RequestPart], _Config) ->
    finish_exception(client_closed, {request_part, RequestPart});
handle_event(client_timeout, [RequestPart], _Config) ->
    finish_exception(client_timeout, {request_part, RequestPart});
handle_event(bad_request, [Reason], _Config) ->
    finish_exception(bad_request, Reason);
handle_event(request_error, [_Req, Exception, Stacktrace], _Config) ->
    finish_exception(Exception, Stacktrace);
handle_event(request_throw, [_Req, Exception, Stacktrace], _Config) ->
    finish_exception(Exception, Stacktrace);
handle_event(request_exit, [_Req, Exception, Stacktrace], _Config) ->
    finish_exception(Exception, Stacktrace);
handle_event(_Event, _Args, _Config) ->
    ok.

%%

handle_full_response(_Type, [_Req, Code, _Hs, _B, {Timings, Sizes}], _Config) ->
    ocp:update_tags(#{http_server_status => integer_to_list(Code)}),

    case proplists:get_value(req_body, Sizes) of
        undefined ->
            ok;
        UncompressedReqSize ->
            ocp:record('opencensus.io/http/server/received_bytes', UncompressedReqSize),
            ocp:add_time_event(oc_trace:message_event(?MESSAGE_EVENT_TYPE_RECEIVED, 0,
                                                      UncompressedReqSize, UncompressedReqSize))
    end,
    ServerLatency = proplists:get_value(request_end, Timings) - proplists:get_value(headers_start, Timings),
    ocp:record('opencensus.io/http/server/server_latency', ServerLatency),

    UncompressedRespSize = size(Sizes, response_body),
    ocp:record('opencensus.io/http/server/sent_bytes', UncompressedRespSize),

    ocp:add_time_event(oc_trace:message_event(?MESSAGE_EVENT_TYPE_SENT, 0, UncompressedRespSize, UncompressedRespSize)),
    ocp:put_attribute(<<"http.status_code">>, Code),
    ocp:set_status(opencensus:http_status_to_trace_status(Code), <<>>),
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

finish_exception(Exception, Stacktrace) ->
    ocp:put_attributes(#{<<"stacktrace">> => term_to_string(Stacktrace),
                         <<"error.message">> => term_to_string(Exception)}).

to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
to_binary(Method) ->
    Method.

term_to_string(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

