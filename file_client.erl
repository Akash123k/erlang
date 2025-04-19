-module(file_client).
-export([start/0, start_client/1, client/1]).

start_client(Request) ->
    spawn(?MODULE, client, [Request]).


start() ->
    io:format("~n--- welcome  to AK Files system ---~n"),
    loop().

loop() ->
    io:format("~n1. create file~n"),
    io:format("2  write in file~n"),
    io:format("3  read file~n"),
    io:format("4 append to file~n"),
    io:format("5 delete file~n"),
    io:format("6  rename file~n"),
    io:format("7  check if file exists~n"),
    io:format("8 get file info~n"),
    io:format("9 list directory~n"),
    io:format("10  move file~n"),
    io:format("0 exit~n"),
    io:format("enter choice"),


    case io:fread("", "~d") of
        {ok, [Choice]} ->
            case Choice of
                0 -> io:format("Exiting...~n"), halt();
                _ -> handle_choice(Choice), loop()
            end;
        _ ->
            io:format("invalid choice plz try again .~n"),


            loop()
    end.








client(Request) ->
    {my_file_server, 'server@127.0.1.1'} ! {self(), Request},
    receive
        {my_file_server, Response} ->
            io:format("~n[Server Response]: ~p~n", [Response])
    end.


    

handle_choice(1) ->
    File = prompt("enter file name to create: "),
    start_client({create, File});


handle_choice(2) ->
    File = prompt("enter file name to write: "),
    Text = prompt("enter content: "),
    start_client({write, File, Text});


handle_choice(3) ->
    File = prompt("enter file name to read: "),
    start_client({read, File});


handle_choice(4) ->
    File = prompt("enter file name to append: "),
    Text = prompt("enter content to append: "),
    start_client({append, File, Text});


handle_choice(5) ->
    File = prompt("enter file name to delete: "),
    start_client({delete, File});



handle_choice(6) ->
    Old = prompt("enter current file name: "),
    New = prompt("enter new file name: "),
    start_client({rename, Old, New});



handle_choice(7) ->
    File = prompt("enter file name to check existence: "),
    start_client({exists, File});



handle_choice(8) ->
    File = prompt("enter file name to get info: "),
    start_client({info, File});



handle_choice(9) ->
    Dir = prompt("enter directory to list: "),
    start_client({list, Dir});


handle_choice(10) ->
    Src = prompt("enter source file path: "),
    Dst = prompt("enter destination path: "),
    start_client({move, Src, Dst});


handle_choice(_) ->
    io:format("Invalid choice. Try again.~n").



prompt(Message) ->
    io:format("~s", [Message]),
    case io:get_line("") of
        Line -> string:trim(Line)
    end.
