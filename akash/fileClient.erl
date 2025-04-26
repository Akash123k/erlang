-module(fileClient).
-export([create/1, write/2, append/2, read/1, delete/1, rename/2, list/0, exists/1]).
-define(SERVER_NODE, ('server@127.0.0.1')).

send(Request) ->
    {fileServer, ?SERVER_NODE} ! {self(), Request},
    receive
        Reply ->
            io:format("[client] ~p~n", [Reply])
    after 3000 ->
        io:format("[client] no response from server~n")
    end.

create(Name) -> send({create, Name}).
write(Name, Content) -> send({write, Name, Content}).
append(Name, Content) -> send({append, Name, Content}).
read(Name) -> send({read, Name}).
delete(Name) -> send({delete, Name}).
rename(Old, New) -> send({rename, Old, New}).
list() -> send({list, "."}).
exists(Name) -> send({exists, Name}).


