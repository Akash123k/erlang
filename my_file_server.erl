-module(my_file_server).
-export([start_server/0, server/0]).

start_server() ->
    register(my_file_server, spawn(?MODULE, server, [])).


server() ->
    receive
        {From, {create, Filename}} ->
            log("Creating file: ~s", [Filename]),
            Reply = case file:write_file(Filename, <<>>) of
                        ok -> {ok, "File created"};
                        {error, Reason} -> {error, Reason}
                    end,
            From ! {my_file_server, Reply},
            server();





        {From, {write, Filename, Content}} ->
            log("Writing to file: ~s", [Filename]),
            Reply = case file:write_file(Filename, Content) of
                        ok -> {ok, "File written"};
                        {error, Reason} -> {error, Reason}
                    end,
            From ! {my_file_server, Reply},
            server();



        {From, {read, Filename}} ->
            log("Reading file: ~s", [Filename]),
            Reply = case file:read_file(Filename) of
                        {ok, Data} -> {ok, Data};
                        {error, Reason} -> {error, Reason}
                    end,
            From ! {my_file_server, Reply},
            server();



        {From, {append, Filename, Content}} ->
            log("Appending to file: ~s", [Filename]),
            Reply = case file:write_file(Filename, Content, [append]) of
                        ok -> {ok, "Content appended"};
                        {error, Reason} -> {error, Reason}
                    end,
            From ! {my_file_server, Reply},
            server();



        {From, {delete, Filename}} ->
            log("Deleting file: ~s", [Filename]),
            Reply = case file:delete(Filename) of
                        ok -> {ok, "File deleted"};
                        {error, Reason} -> {error, Reason}
                    end,
            From ! {my_file_server, Reply},
            server();




        {From, {rename, Old, New}} ->
            log("Renaming file ~s to ~s", [Old, New]),
            Reply = case file:rename(Old, New) of
                        ok -> {ok, "File renamed"};
                        {error, Reason} -> {error, Reason}
                    end,
            From ! {my_file_server, Reply},
            server();



        {From, {exists, Filename}} ->
            log("Checking existence of: ~s", [Filename]),
            Reply = case file:read_file_info(Filename) of
                        {ok, _} -> {ok, true};
                        _ -> {ok, false}
                    end,
            From ! {my_file_server, Reply},
            server();



        {From, {info, Filename}} ->
            log("Getting info for: ~s", [Filename]),
            Reply = file:read_file_info(Filename),
            From ! {my_file_server, Reply},
            server();




        {From, {list, Dir}} ->
            log("Listing directory: ~s", [Dir]),
            Reply = file:list_dir(Dir),
            From ! {my_file_server, Reply},
            server();




        {From, {move, Source, Dest}} ->
            log("Moving file from ~s to ~s", [Source, Dest]),
            case file:copy(Source, Dest) of
                ok ->
                    file:delete(Source),
                    From ! {my_file_server, {ok, "File moved"}};
                Error ->
                    From ! {my_file_server, Error}
            end,
            server();


            

        {From, done} ->
            log("Shutting down server", []),
            From ! {my_file_server, "Server shutting down"};
        
        {From, Unknown} ->
            log("Received unknown request: ~p", [Unknown]),
            From ! {my_file_server, {error, "Invalid operation"}},
            server()
    end.
