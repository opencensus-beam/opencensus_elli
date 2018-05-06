opencensus_elli
=====

[![CircleCI](https://circleci.com/gh/census-instrumentation/opencensus_elli.svg?style=svg)](https://circleci.com/gh/census-instrumentation/opencensus_elli)
[![codecov](https://codecov.io/gh/census-instrumentation/opencensus_elli/branch/master/graph/badge.svg)](https://codecov.io/gh/census-instrumentation/opencensus_elli)
[![Hex.pm](https://img.shields.io/hexpm/v//opencensus_elli.svg?maxAge=2592000)](https://hex.pm/packages/opencensus_elli)
[![Hex.pm](https://img.shields.io/hexpm/dt/opencensus_elli.svg?maxAge=2592000)](https://hex.pm/packages/opencensus_elli)
        
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

