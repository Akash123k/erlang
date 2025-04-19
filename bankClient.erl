%% bankClient.erl
-module(bankClient).
-export([start/0, loop/0]).

start() ->
    io:format("~n--- Welcome to AK Bank ---~n"),

    loop().
    
loop() ->
    io:format("~n1. Create Account~n"),
    io:format("2. deposit~n"),
    io:format("3. withdraw~n"),
    io:format("4. show Balance~n"),
    io:format("5. total Accounts~n"),
    io:format("6. List All Accounts~n"),
    io:format("7. View Profile~n"),
    io:format("0. Exit~n"),
    io:format("Enter choice: "),
    {ok, [Choice]} = io:fread("", "~d"),
    handle_choice(Choice),
    loop().




handle_choice(0) ->
    io:format("Goodbye!~n"),
    halt();

handle_choice(1) ->
    {ok, [Acc]} = io:fread("Enter new account number: ", "~s"),
    {ok, [Name]} = io:fread("Enter name: ", "~s"),
    {ok, [Address]} = io:fread("Enter address: ", "~s"),
    {ok, [Gender]} = io:fread("Enter gender (M/F): ", "~s"),
    {ok, [Aadhaar]} = io:fread("Enter Aadhaar number: ", "~s"),
    {ok, [Pan]} = io:fread("Enter PAN number: ", "~s"),
    send({create_account, list_to_atom(Acc), list_to_binary(Name), list_to_binary(Address), list_to_binary(Gender), list_to_binary(Aadhaar), list_to_binary(Pan)});


handle_choice(2) ->
    {ok, [Acc]} = io:fread("Enter account number: ", "~s"),
    {ok, [Amt]} = io:fread("Enter amount to deposit: ", "~d"),
    send({deposit, list_to_atom(Acc), Amt});

handle_choice(3) ->
    {ok, [Acc]} = io:fread("Enter account number: ", "~s"),
    {ok, [Amt]} = io:fread("Enter amount to withdraw: ", "~d"),
    send({withdraw, list_to_atom(Acc), Amt});

handle_choice(4) ->
    {ok, [Acc]} = io:fread("Enter account number: ", "~s"),
    send({balance, list_to_atom(Acc)});
handle_choice(5) ->
    {ok, [Key]} = io:fread("Enter admin key: ", "~s"),
    send({get_account_count, list_to_binary(Key)});

handle_choice(6) ->
    {ok, [Key]} = io:fread("Enter admin key: ", "~s"),
    send({get_all_accounts, list_to_binary(Key)});

    
handle_choice(7) ->
    {ok, [Acc]} = io:fread("Enter account number to view profile: ", "~s"),
    send({view_profile, list_to_atom(Acc)});



handle_choice(_) ->
    io:format("Invalid choice.~n").

send(Request) ->
    Server = {bankServer, 'server@127.0.1.1'}, 
    Server ! {self(), Request},
    receive
        {bankServer, Response} ->
            io:format("server replied: ~p~n", [Response])
    after 3000 ->
        io:format("timeout: server not responding~n")
    end.
