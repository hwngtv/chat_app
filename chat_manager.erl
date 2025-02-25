-module(chat_manager).
-export([create_message_table/0, send_message/2, get_messages/0]).

create_message_table() ->
    mnesia:create_table(messages, [{disc_copies, [node()]}, {attributes, [sender, content, timestamp]}]).

send_message(Sender, Content) ->
    Timestamp = erlang:system_time(),
    F = fun() ->
        mnesia:write({messages, Sender, Content, Timestamp}),
        {ok, "Message sent"}
    end,
    mnesia:transaction(F).

get_messages() ->
    F = fun() ->
        mnesia:match_object({messages, '_'})
    end,
    mnesia:transaction(F).
