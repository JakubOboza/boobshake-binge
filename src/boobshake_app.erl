-module(boobshake_app).

-behaviour(application).

%% Defines
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(PORT, 3666).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================


% First spawn worker then start blocking socket loop

start(_StartType, _StartArgs) ->
    boobshake_sup:start_link(),
    {ok, Socket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    accept_loop(Socket).

stop(_State) ->
    ok.

accept_loop(Socket) ->
    {ok , ClientSocket} = gen_tcp:accept(Socket),
    spawn(fun() -> client_loop(ClientSocket) end),
    accept_loop(Socket).

client_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
      {ok, Data} ->
            [StrippedData] = string:tokens(binary:bin_to_list(Data), "\r\n"),
            Response = try mochijson2:decode(binary:list_to_bin(StrippedData)) of
              DecodedJson -> process_request(DecodedJson)
            catch
              _:_ -> {error, "Could not process request"}
            end,
            {Status, Value} = Response,
            gen_tcp:send(Socket, binary:list_to_bin(mochijson2:encode( [binary:list_to_bin(atom_to_list(Status)), binary:list_to_bin(Value) ] ) ) ),
            client_loop(Socket);
      {error, closed} ->
            ok
    end.

process_request({struct, [{<<"peek">>, Key}]}) -> memory_storage:peek( binary:bin_to_list(Key) );
process_request({struct, [{<<"set">>, {struct,[{Key, Val}]}}]}) -> memory_storage:set(binary:bin_to_list(Key), binary:bin_to_list(Val) );
process_request(_) -> {error, "Could not process request"}.



-ifdef(TEST).

process_request_error_test() ->
    {error, _} = process_request({}),
    {error, _} = process_request(kinky_atom).

process_request_test() ->
     memory_storage:start_link(),
    {ok, _} = process_request({struct, [{<<"set">>, {struct,[{<<"test-key">>, <<"test-val">>}]}}]}),
    {ok, "test-val"} = process_request({struct, [{<<"peek">>, <<"test-key">>}]}).

app_test() ->
    _ = spawn(fun() -> application:start(boobshake) end).

-endif.
