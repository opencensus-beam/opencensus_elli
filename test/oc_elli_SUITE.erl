-module(oc_elli_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("opencensus/include/opencensus.hrl").
-include_lib("elli/include/elli.hrl").

all() ->
    [successful_request, error_response].

init_per_suite(Config) ->
    ok = application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    application:set_env(opencensus, sampler, {oc_sampler_always, []}),
    application:set_env(opencensus, send_interval_ms, 1),
    application:set_env(opencensus, reporters, [{oc_reporter_pid, self()}]),
    {ok, _} = application:ensure_all_started(opencensus),

    elli:start_link([{port, 3000},
                     {callback, elli_middleware},
                     {callback_args, [{mods, [{oc_elli_middleware, []},
                                              {?MODULE, []}]}]}]),
    Config.

end_per_testcase(_, _Config)->
    application:stop(opencensus),
    ok.

successful_request(_Config) ->
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request("http://localhost:3000"),
    ?assertEqual("Hello World!", Body),

    receive
        {span, #span{name=Name,
                     attributes=Attributes,
                     time_events=TimeEvents}} ->
            ?assertEqual(<<"/">>, Name),
            ?assertMatch(#{<<"http.url">> := <<"/">>,
                           <<"http.host">> := <<"localhost:3000">>,
                           <<"http.status_code">> := 200,
                           <<"http.user_agent">> := <<>>,
                           <<"http.method">> := <<"GET">>}, Attributes),
            ?assertMatch([{_, #message_event{type=?MESSAGE_EVENT_TYPE_SENT,
                                             compressed_size=12,
                                             uncompressed_size=12}},
                          {_, #message_event{type=?MESSAGE_EVENT_TYPE_RECEIVED,
                                             compressed_size=0,
                                             uncompressed_size=0}}], TimeEvents)
    after
        5000 ->
            error(timeout)
    end,
    ok.

error_response(_Config) ->
    {ok, {{_, 500, _}, _Headers, _Body}} = httpc:request("http://localhost:3000/error"),

    receive
        {span, #span{name=Name,
                     attributes=Attributes}} ->
            ?assertEqual(<<"/error">>, Name),
            ?assertMatch(#{<<"http.url">> := <<"/error">>,
                           <<"http.host">> := <<"localhost:3000">>,
                           <<"http.status_code">> := 500,
                           <<"http.user_agent">> := <<>>,
                           <<"error.message">> := <<"all_hell">>,
                           <<"stacktrace">> := _,
                           <<"http.method">> := <<"GET">>}, Attributes)
    after
        5000 ->
            error(timeout)
    end,
    ok.
%%


handle(Req, _Args) ->
    case Req#req.raw_path of
        <<"/">> ->
            {ok, [], <<"Hello World!">>};
        <<"/error">> ->
            throw(all_hell)
    end.

handle_event(_Event, _Data, _Args) ->
    ok.
