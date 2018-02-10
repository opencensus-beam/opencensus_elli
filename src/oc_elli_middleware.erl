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
         handle_event/3]).

preprocess(Req=#req{raw_path=RawPath, method=Method}, _) ->
    case elli_request:get_header(<<"X-Cloud-Trace-Context">>, Req, undefined) of
        undefined ->
            ocp:start_trace();
        TCHeader ->
            ocp:start_trace(oc_trace_context_headers:decode(TCHeader))
    end,

    Operation = <<(to_binary(Method))/binary, ":", RawPath/binary>>,
    ocp:start_span(Operation),
    ocp:put_attributes(attributes(Req)),
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

handle_full_response(_Type, [_Req, Code, _Hs, _B, {_Timings, Sizes}], __Config) ->
    ocp:put_attribute(<<"http.status">>, integer_to_binary(Code)),
    ResponseSize = size(Sizes, response_body),
    ocp:put_attribute(<<"http.response_size">>, integer_to_binary(ResponseSize)),
    ocp:finish_span(),
    ok.

size(Sizes, response) ->
    size(Sizes, response_headers) +
        size(Sizes, response_body);
size(Sizes, response_headers) ->
    proplists:get_value(resp_headers, Sizes);
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
    ocp:put_attributes(#{<<"stacktrace">> => term_to_string(Stacktrace),
                         <<"error.message">> => term_to_string(Exception)}),
    ocp:finish_span().

attributes(Req=#req{raw_path=Path,
                    method=Method}) ->
    Host = elli_request:get_header(<<"Host">>, Req, <<>>),
    UserAgent = elli_request:get_header(<<"User-Agent">>, Req, <<>>),
    maps:from_list([{<<"http.url">>, Path},
                    {<<"http.host">>, Host},
                    {<<"http.user_agent">>, UserAgent},
                    {<<"http.method">>, to_binary(Method)}]).

to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
to_binary(Method) ->
    Method.

term_to_string(Term) ->
    io_lib:format("~p", [Term]).
