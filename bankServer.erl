%% bankServer.erl
-module(bankServer).
-export([start_server/0, server/1]).

start_server() ->
    register(bankServer, spawn(bankServer, server, [#{}])).

server(Accounts) ->
    receive
        {From, {create_account, AccountNo, Name, Address, Gender, Aadhaar, Pan}} ->
            case maps:is_key(AccountNo, Accounts) of
                true ->
                    From ! {bankServer, {error, already_exists}};
                false ->
                    NewAccounts = Accounts#{AccountNo => {Name, Address, Gender, Aadhaar, Pan, 0}},
                    From ! {bankServer, {ok, account_created}},
                    server(NewAccounts)
            end;




        {From, {deposit, AccountNo, Amount}} when is_number(Amount), Amount > 0 ->
            case maps:find(AccountNo, Accounts) of
                {ok, {Name, Address, Gender, Aadhaar, Pan, Bal}} ->
                    NewBal = Bal + Amount,
                    NewAccounts = Accounts#{AccountNo => {Name, Address, Gender, Aadhaar, Pan, NewBal}},
                    From ! {bankServer, {ok, NewBal}},
                    server(NewAccounts);
                error ->
                    From ! {bankServer, {error, account_not_found}},
                    server(Accounts)
            end;




        {From, {withdraw, AccountNo, Amount}} when is_number(Amount), Amount > 0 ->
            case maps:find(AccountNo, Accounts) of
                {ok, {Name, Address, Gender, Aadhaar, Pan, Bal}} when Bal >= Amount ->
                    NewBal = Bal - Amount,
                    NewAccounts = Accounts#{AccountNo => {Name, Address, Gender, Aadhaar, Pan, NewBal}},
                    From ! {bankServer, {ok, NewBal}},
                    server(NewAccounts);
                {ok, _} ->
                    From ! {bankServer, {error, insufficient_funds}},
                    server(Accounts);
                error ->
                    From ! {bankServer, {error, account_not_found}},
                    server(Accounts)
            end;





        {From, {view_profile, AccountNo}} ->
            case maps:find(AccountNo, Accounts) of
                {ok, {Name, Address, Gender, Aadhaar, Pan, Bal}} ->
                    From ! {bankServer, {profile, Name, Address, Gender, Aadhaar, Pan, Bal}};
                error ->
                    From ! {bankServer, {error, account_not_found}},
                    server(Accounts)
            end;







        {From, {get_account_count, AdminKey}} ->
            case AdminKey of
                <<"pucsd">> ->
                    Count = maps:size(Accounts),
                    From ! {bankServer, {account_count, Count}};
                _ ->
                    From ! {bankServer, {error, unauthorized}}
            end,
            server(Accounts);

        {From, {get_all_accounts, AdminKey}} ->
            case AdminKey of
                <<"pucsd">> ->
                    Keys = maps:keys(Accounts),
                    From ! {bankServer, {all_accounts, Keys}};
                _ ->
                    From ! {bankServer, {error, unauthorized}}
            end,
            server(Accounts);




        {From, {balance, AccountNo}} ->

            case maps:find(AccountNo, Accounts) of
                {ok, {Name, Address, Gender, Aadhaar, Pan, Bal}} ->
                    From ! {bankServer, {balance, Bal, Name, Address, Gender, Aadhaar, Pan}}; 
                error ->
                    From ! {bankServer, {error, account_not_found}}
            end,
            server(Accounts);

        {From, done} ->
            From ! {bankServer, "session ended"};

        {From, _Other} ->
            From ! {bankServer, "invalid request"},
            server(Accounts)




            
    end.
