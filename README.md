opencensus_elli
=====

Elli middleware for [OpenCensus](http://opencensus.io/) instrumentation.

```erlang
{deps, [opencensus_elli]}.
```

Using the `elli_middleware` callback place `oc_elli_middelware` as the first module to be called in the list of handlers:

```erlang
[{callback, elli_middleware},
 {callback_args, [{mods, [{oc_elli_middleware, []},
                          {<YOUR HANDLER>, []}]}]}]
```

