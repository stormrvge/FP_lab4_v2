-module(chat_server).
-behaviour(gen_server).

%% API
-export([start/0, send/1, accept/3, refresh_connections/2, get_connections/1,  handshake/1, ping/1, print_history/0, get_messages/1, leave/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, insert_message/2]).

-record(state, {connections = [], ets = undefined, public_key = undefined, private_key = undefined}).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Message) ->
  gen_server:cast(?MODULE, {send, Message}).

insert_message(User, Message) ->
  gen_server:cast(User, {insert_message, {User, Message}}).

refresh_connections(User, Connections) ->
  gen_server:cast(User, {refresh_connections, Connections}).

handshake(User) ->
  gen_server:call(User, {handshake}).

accept(Sender, User, Message) ->
  gen_server:cast(User, {chat_message, Sender, Message}).

get_messages(User) ->
  gen_server:call(User, get_messages).

get_connections(User) ->
  gen_server:call(User, get_connections).

print_history() ->
  gen_server:cast(?MODULE, print_history).

ping(User) -> gen_server:call(User, ping).

leave(User) ->
  gen_server:call(?MODULE, {leave, User}).

%% gen_server callbacks
init([]) ->
  {PublicKey, PrivateKey} = crypto:generate_key(rsa, {1024, 3}),

  NewEtsTable = ets:new(messages, [public, named_table, {keypos, 2}, duplicate_bag]),

  try
    {Connections, EtsTableList} = discovery_server:register(self()),
    case EtsTableList of
      undefined ->
        {ok, #state{connections = Connections, ets = NewEtsTable, public_key = PublicKey, private_key = PrivateKey}};
      _ ->
        lists:foreach(
          fun(Object) ->
            ets:insert(NewEtsTable, Object)
          end,
          EtsTableList
        ),
        {ok, #state{connections = Connections, ets = NewEtsTable, public_key = PublicKey, private_key = PrivateKey}}
    end
  catch
    _:_ ->
      Pids = read_pids(),
      User = find_available_user(Pids),
      NewConnections = chat_server:get_connections(User),
      EtsList = chat_server:get_messages(User),
      lists:foreach(
        fun(Object) ->
          ets:insert(NewEtsTable, Object)
        end,
        EtsList
      ),
      {ok, #state{connections = NewConnections, ets = NewEtsTable, public_key = PublicKey, private_key = PrivateKey}}
  end.

handle_call(get_messages, _From, State) ->
  {reply, ets:tab2list(State#state.ets), State};

handle_call(get_connections, _From, State) ->
  {reply, State#state.connections, State};

handle_call(ping, _From, State) ->
  {reply, {ok, "Connection can be established"}, State};

handle_call({leave, User}, _From, State) ->
  save_data(State#state.connections),
  {reply, ok, State#state{connections = lists:delete(User, State#state.connections)}};

handle_call({handshake}, _From, #state{connections = Users, ets = Ets, public_key = PublicKey, private_key = PrivateKey}) ->
  {reply, PublicKey, #state{connections = Users, ets = Ets, public_key = PublicKey, private_key = PrivateKey}}.

handle_cast({send, Message}, State) ->
  Timestamp = erlang:system_time(millisecond),
  ets:insert_new(State#state.ets, {{self(), Timestamp}, Message}),
  send_to_all(Message, State#state.connections),
  {noreply, State};

handle_cast({chat_message, From, EncryptedMessage}, State) ->
  DecryptedMessage = crypto:private_decrypt(rsa, EncryptedMessage, State#state.private_key, []),
  Timestamp = erlang:system_time(millisecond),
  ets:insert_new(State#state.ets, {{From, Timestamp}, DecryptedMessage}),
  FormattedTime = to_formatted_time(Timestamp),
  io:format("[~s] - From: ~w Message: ~s~n", [FormattedTime, From, binary_to_list(DecryptedMessage)]),
  {noreply, State};

handle_cast({refresh_connections, Connections}, #state{connections = _, ets = Ets, public_key = PublicKey, private_key = PrivateKey}) ->
  {noreply, #state{connections = Connections, ets = Ets, public_key = PublicKey, private_key = PrivateKey}};

handle_cast({insert_message, {User, Message}}, State) -> % Update the pattern match
  Timestamp = erlang:system_time(millisecond),
  ets:insert_new(State#state.ets, {{User, Timestamp}, Message}),
  {noreply, State};

handle_cast(print_history, State) ->
  EtsList = ets:tab2list(State#state.ets),

  SortedEtsList = lists:sort(fun({{_, Timestamp1}, _}, {{_, Timestamp2}, _}) -> Timestamp1 =< Timestamp2 end, EtsList),

  io:format("The history~n"),

  _ = lists:foreach(
    fun({{From, Timestamp}, Message}) ->
      FormattedTime = to_formatted_time(Timestamp),
      io:format("[~s] From ~w Message ~s~n", [FormattedTime, From, Message])
    end,
    SortedEtsList
  ),

  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Helper functions
send_to_all(Message, Users) ->
  lists:foreach(fun(Other) ->
    UserPublicKey = chat_server:handshake(Other),
    EncryptedMessage = crypto:public_encrypt(rsa, list_to_binary(Message), UserPublicKey, []),
    chat_server:accept(self(), Other, EncryptedMessage)
                end,
    Users),
  chat_server:insert_message(self(), Message).

to_formatted_time(Timestamp) ->
  UTCTime = calendar:system_time_to_universal_time(Timestamp, millisecond),
  LocalTime = calendar:universal_time_to_local_time(UTCTime),
  format_time(LocalTime).

format_time({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B|~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second]).

read_pids() ->
  {ok, Fd} = file:open("peers", [read]),
  try get_all_lines(Fd, []) after file:close(Fd) end.


get_all_lines(Device, Lines) ->
  case io:get_line(Device, "") of
    eof -> Lines;
    Line -> get_all_lines(Device, [string:chomp(Line) | Lines])
  end.

find_available_user([]) -> {error, "Unable to find avaible user"};

find_available_user(Pids) ->
  StringPid = lists:nth(rand:uniform(length(Pids)), Pids),
  Pid = erlang:list_to_pid(StringPid),
  try chat_server:ping(Pid) of
    {ok, _} -> Pid
  catch
    _:_ -> find_available_user(lists:delete(StringPid, Pids))
  end.

save_data(Connections) ->
  {ok, Fd} = file:open("peers", [write]),
  lists:foreach(
    fun (Connection) -> file:write(Fd, io_lib:fwrite("~p\n", [Connection])) end,
    Connections
  ).