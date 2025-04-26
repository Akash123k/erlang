-module(fileServer).
-export([start_server/0, server/0]).

start_server() ->
    register(fileServer, spawn(fileServer, server, [])).

server() ->
    receive
        {From, Request} ->
            io:format("[server] received request: ~p~n", [Request]),
            request(From, Request),
            server();
        _Other ->
            io:format("[server] unknown message received~n"),
            server()
    end.


request(From, {create, FileName}) ->
    case file:open(FileName, [write]) of
        {ok, IoDevice} ->
            file:close(IoDevice),
            From ! {fileServer, {ok, created}};
        Error ->
            From ! {fileServer, {error, Error}}
    end;


request(From, {write, FileName, Content}) ->
    case file:open(FileName, [write]) of
        {ok, IoDevice} ->
            ok = io:put_chars(IoDevice, Content),
            file:close(IoDevice),
            From ! {fileServer, {ok, written}};
        Error ->
            From ! {fileServer, {error, Error}}
    end;



request(From, {append, FileName, Content}) ->
    case file:open(FileName, [append]) of
        {ok, IoDevice} ->
            ok = io:put_chars(IoDevice, Content),
            file:close(IoDevice),
            From ! {fileServer, {ok, appended}};
        Error ->
            From ! {fileServer, {error, Error}}
    end;



request(From, {read, FileName}) ->
    case file:read_file(FileName) of
        {ok, Binary} ->
            From ! {fileServer, {ok, binary_to_list(Binary)}};
        Error ->
            From ! {fileServer, {error, Error}}
    end;



request(From, {delete, FileName}) ->
    Result = file:delete(FileName),
    From ! {fileServer, Result};




request(From, {rename, Old, New}) ->
    Result = file:rename(Old, New),
    From ! {fileServer, Result};




request(From, {exists, FileName}) ->
    case filelib:is_file(FileName) of
        true -> From ! {fileServer, {ok, exists}};
        false -> From ! {fileServer, {error, not_found}}
    end;




request(From, {list, Dir}) ->
    case file:list_dir(Dir) of
        {ok, Files} -> From ! {fileServer, {ok, Files}};
        Error -> From ! {fileServer, {error, Error}}
    end;




request(From, done) ->
    io:format("[server] session ended~n"),
    From ! {fileServer, "session ended"};



request(_, _) ->
    io:format("[server] unknown request~n").
