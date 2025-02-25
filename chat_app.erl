-module(chat_app).
-export([start/0, stop/0]).

start() ->
    % Kiểm tra schema, nếu không tồn tại thì tạo mới
    case file:read_file_info("/home/hwngtv/Documents/test/chatApp/Mnesia.nonode@nohost") of
        {ok, _} -> io:format("Schema already exists~n");
        _ -> mnesia:create_schema([node()])
    end,
    mnesia:start(),
    user_manager:create_user_table(),
    chat_manager:create_message_table(),
    chat_server:start_link(),
    io:format("Chat server started~n").

stop() ->
    mnesia:stop(),
    io:format("Chat server stopped~n").
