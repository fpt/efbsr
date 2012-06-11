%% @author Youichi FUJIMOTO <yofujimo@gmail.com>
%% @copyright 2012 Youichi Fujimoto, All rights reserved. Open source, BSD License.
%% @doc jsonconv: Data structure converter for mochijson2:decode

-module(jsonconv).
-author('author <yofujimo@gmail.com>').
-export([conv/1]).


% mochijson2 list or obj -> list or obj
conv(Json) ->
    if
        is_list(Json) ->
            lists:map(fun conv_single/1, Json);
        true ->
            conv_single(Json)
    end.

% KV -> KV
conv_recur({K, {struct, Kv}}) ->
    {binary_to_list(K), lists:map(fun conv_recur/1, Kv)};
conv_recur({K, V}) ->
    if
        is_binary(V) ->
            {binary_to_list(K), binary_to_list(V)};
        is_list(V) ->
            {binary_to_list(K), conv(V)};
        true ->
            {binary_to_list(K), V}
    end.

% list -> list
conv_single({struct, Kv}) ->
    lists:map(fun conv_recur/1, Kv);
conv_single(V) ->
    if
        is_binary(V) ->
            binary_to_list(V);
        is_list(V) ->
            lists:map(fun conv_single/1, V);
        true ->
            V
    end.
