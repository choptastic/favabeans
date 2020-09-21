%%%-------------------------------------------------------------------
%% @doc favabeans public API
%% @end
%%%-------------------------------------------------------------------

-module(favabeans_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    favabeans_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
