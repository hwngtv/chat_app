-module(chat_server).
-behaviour(gen_server).

%% API
-export([start_link/0, send_message/2, get_messages/0, register_user/2, login_user/2, logout_user/1]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {users, logged_in_users, messages}).

%% Khởi động GenServer
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% GenServer callback init/1
init(_) ->
    % Trạng thái ban đầu của GenServer bao gồm danh sách người dùng đăng nhập và các tin nhắn đã gửi
    {ok, #state{users = [], logged_in_users = [], messages = []}}.

%% API đăng ký user
register_user(Username, Password) ->
    gen_server:call(?MODULE, {register, Username, Password}).

%% API đăng nhập user
login_user(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

%% API đăng xuất user
logout_user(Username) ->
    gen_server:call(?MODULE, {logout, Username}).

%% API gửi tin nhắn, chỉ khi đã đăng nhập
send_message(Sender, Content) ->
    gen_server:cast(?MODULE, {send_message, Sender, Content}).

%% API lấy tin nhắn
get_messages() ->
    gen_server:call(?MODULE, get_messages).

%% Xử lý các request của GenServer
handle_call({register, Username, Password}, _From, State) ->
    Result = user_manager:register(Username, Password),
    {reply, Result, State};

handle_call({login, Username, Password}, _From, State) ->
    case user_manager:login(Username, Password) of
        {ok, _Msg} ->
            % Đăng nhập thành công, thêm vào danh sách người dùng đã đăng nhập
            NewState = State#state{logged_in_users = lists:usort([Username | State#state.logged_in_users])},
            {reply, {ok, "Login successful"}, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call({logout, Username}, _From, State) ->
    % Xóa người dùng khỏi danh sách đăng nhập
    NewState = State#state{logged_in_users = lists:delete(Username, State#state.logged_in_users)},
    {reply, {ok, "Logout successful"}, NewState};

handle_call(get_messages, _From, State) ->
    Messages = chat_manager:get_messages(),
    {reply, Messages, State}.

%% Xử lý gửi tin nhắn không đồng bộ (cast)
handle_cast({send_message, Sender, Content}, State) ->
    % Chỉ gửi tin nhắn nếu người dùng đã đăng nhập
    case lists:member(Sender, State#state.logged_in_users) of
        true ->
            % Lưu tin nhắn và gửi cho tất cả người dùng đã đăng nhập
            Timestamp = erlang:system_time(),
            NewMessage = {Sender, Content, Timestamp},
            NewState = State#state{messages = [NewMessage | State#state.messages]},
            broadcast_message(Sender, Content, State#state.logged_in_users),
            {noreply, NewState};
        false ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

%% Gửi tin nhắn đến tất cả người dùng đã đăng nhập
broadcast_message(_Sender, _Content, []) ->
    ok;
broadcast_message(Sender, Content, [User | Rest]) ->
    io:format("Message from ~s to ~s: ~s~n", [Sender, User, Content]),
    broadcast_message(Sender, Content, Rest).
