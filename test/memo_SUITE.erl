%%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% Copyright (c) 2015-2018 Tuncer Ayaz
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(memo_SUITE).

-export(
   [
    all/0,
    init_per_suite/1,
    end_per_suite/1
   ]).

-export(
   [
    call/1,
    failed_call/1,
    fun_obj_call/1,
    manual_lookup/1
   ]).

all() ->
    [
     call,
     failed_call,
     manual_lookup
    ].

init_per_suite(Config) ->
    ok = application:start(memo),
    Config.

end_per_suite(_Config) ->
    ok.

call(_Config) ->
    Res = [1, 2],
    Args = [[1], [2]],
    %% First call with caching
    Res = memo:call(lists, append, Args),
    %% Second call with cache lookup
    Res = memo:call(lists, append, Args).

fun_obj_call(_Config) ->
    Res = [6, 7],
    Args = [[6], [7]],
    Append = fun(L1, L2) -> lists:append(L1, L2) end,
    Res = memo:call(Append, Args),
    Res = memo:call(fun lists:append/2, Args).

failed_call(_Config) ->
    %% First call with caching
    {'EXIT', {badarg, _}} = (catch memo:call(lists, append, [3, 4])),
    %% Check that the call wasn't memoized
    Key = erlang:phash2({lists, append, [3, 4]}),
    [] = ets:lookup(memo, Key).

manual_lookup(_Config) ->
    Res = [5, 6],
    Args = [[5], [6]],
    %% First call with caching
    Res = memo:call(lists, append, Args),
    %% Second call with cache lookup
    Res = memo:call(lists, append, Args),
    %% Manual lookup in ets table
    Key = erlang:phash2({lists, append, Args}),
    [{Key, Res}] = ets:lookup(memo, Key).
