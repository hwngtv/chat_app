-module(user_manager).
-export([create_user_table/0, register/2, login/2, logout/1]).

-record(user, {username, password}).

create_user_table() ->
    mnesia:create_table(users, [{disc_copies, [node()]}, 
                                {attributes, record_info(fields, user)}]).

register(Username, Password) ->
    F = fun() ->
        case mnesia:read({users, Username}) of
            [] -> % Nếu username chưa tồn tại, tạo mới với khóa đúng
                mnesia:write({users, Username, #user{username = Username, password = Password}}),
                {ok, "User registered successfully"};
            _ -> % Nếu username đã tồn tại, trả về lỗi
                {error, "Username already taken"}
        end
    end,
    mnesia:transaction(F).

login(Username, Password) ->
    F = fun() ->
        case mnesia:read({users, Username}) of
            [{users, Username, #user{password = P}}] when P =:= Password ->
                {ok, "Login successful"};
            _ ->
                {error, "Invalid credentials"}
        end
    end,
    mnesia:transaction(F).

logout(Username) ->
    io:format("User ~s logged out~n", [Username]),
    {ok, "Logout successful"}.
