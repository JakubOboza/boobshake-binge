%% TODO: Bring DETS tot his callback module

-module(disk_storage).

-behaviour(gen_server).

-export([start_link/0, peek/1, set/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Storage = ets:new('disk_storage', [set]),
    {ok, Storage}.

peek(Key) ->
    gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).


%% callbacks

handle_call({set, Key, Value}, _From, Storage) ->
    Response = case ets:insert(Storage, {Key, Value}) of
      true -> { ok };
      false -> {error, "Could not add pair to storage"}
    end,
    {reply, Response, Storage};

handle_call({get, Key}, _From, Storage) ->
    Response = case ets:lookup(Storage, Key) of
        [] ->
            {error, "No such value"};
        [{Key, Value}] ->
            {ok, Value}
    end,
    {reply, Response, Storage};

% don't break compilation!

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-ifdef(TEST).

disk_test() ->
    disk_storage:start_link(),
    { ok } = disk_storage:set("test-key", "test-value"),
    { ok, "test-value"} = disk_storage:peek("test-key").

-endif.
