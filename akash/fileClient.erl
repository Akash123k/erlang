-module(fileClient).
-export([create/1, write/2, append/2, read/1, delete/1, rename/2, list/0, exists/1]).
-define(SERVER_NODE, ('server@https://97de-196-1-114-253.ngrok-free.app')).

send_request(Request) ->
    {fileServer, ?SERVER_NODE} ! {self(), Request},
    receive
        Reply ->
            io:format("[client] ~p~n", [Reply])
    after 3000 ->
        io:format("[client] no response from server~n")
    end.

create(Name) -> send_request({create, Name}).
write(Name, Content) -> send_request({write, Name, Content}).
append(Name, Content) -> send_request({append, Name, Content}).
read(Name) -> send_request({read, Name}).
delete(Name) -> send_request({delete, Name}).
rename(Old, New) -> send_request({rename, Old, New}).
list() -> send_request({list, "."}).
exists(Name) -> send_request({exists, Name}).


