-module(discovery_server).
-behaviour(gen_server).

%% API
-export([start/0, register/1, unregister/1, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users = []}).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(User) ->
  gen_server:call({?MODULE, node()}, {register, User}).

unregister(User) ->
  gen_server:call({?MODULE, node()}, {unregister, User}).

list() ->
  gen_server:call({?MODULE, node()}, list).

%% gen_server callbacks
init([]) ->
  {ok, #state{users = []}}.

handle_call({register, User}, _From, State) ->
  NewUsers = [User | State#state.users],
  lists:foreach(
    fun(Connection) ->
      chat_server:refresh_connections(Connection, lists:delete(Connection, NewUsers))
    end,
    State#state.users
  ), 

  case State#state.users of 
    [] -> {reply, {State#state.users, undefined}, State#state{users = NewUsers}};
    _ ->
      RandomUser = lists:nth(rand:uniform(length(State#state.users)), State#state.users),
      ChatHistory = chat_server:get_messages(RandomUser),
      {reply, {State#state.users, ChatHistory}, State#state{users = NewUsers}}
  end;

handle_call({unregister, User}, _From, State) ->
  {reply, ok, State#state{users = lists:delete(User, State#state.users)}};

handle_call(list, _From, State) ->
  {reply, State#state.users, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.