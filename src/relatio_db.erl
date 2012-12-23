-module(relatio_db).
-export([up/0, down/0]).
-export([ lookup/2
        , maybe_lookup/2
        , read/2
        , write/1
        , update_with/3
        , remove/2
        , record_to_id/1
        , match_object/1

        , select/1
        , select1/1
        , transaction/1]).

-include_lib("relatio/src/relatio.hrl").


%% -----------------------------------------------------------------------
%% Deploy
%% -----------------------------------------------------------------------

up() ->
    stopped = mnesia:stop(),
    case mnesia:create_schema([node()]) of
        {error,{_Node,{already_exists,_Node}}} ->
            ok = mnesia:start(),
            ok;
        ok -> 
            error_logger:info_msg("Create tables.~n", []),
            
            ok = mnesia:start(),

            mnesia:create_table(r_node_set,
                [{type, ordered_set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, r_node_set)}
                ]),

            Res = mnesia:create_table(r_counter,
                [{type, set}
                ,{disc_copies, [node()]}
                ,{attributes, record_info(fields, r_counter)}
                ]),

            case Res of
                ok -> 
                    [mnesia:write(T, #r_counter{table = T, last_id = 0})
                     || T <- tables(), T =/= r_counter];
                _Other ->
                    %% Table was already created.
                    ok
            end
    end,

    ok = mnesia:wait_for_tables(tables(), 3000),
    ok.


down() ->
    [mnesia:delete_table(X) || X <- tables()],
    ok.


tables() ->
    [r_node_set, r_counter].



%% -----------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------

next_id(Tab) ->
    PrevN = mnesia:dirty_update_counter(r_counter, Tab, #r_counter.last_id),
    PrevN + 1.


write(Rec) ->
    Id = record_to_id(Rec),
    case Id of
        undefined ->
            insert(Rec);
        _Other ->
            update(Rec)
    end.


%% New record
insert(Rec) ->
    Table = erlang:element(1, Rec),
    F = fun() ->
        NextId = next_id(Table),
        Rec2 = erlang:setelement(2, Rec, NextId),
        mnesia:write(Rec2),
        Rec2
        end,
    case mnesia:is_transaction() of
        false ->
            {atomic, Rec3} = mnesia:transaction(F),
            Rec3;
        true ->
            F()
    end.


%% Update
update(Rec) ->
    case mnesia:is_transaction() of
        false ->
            F = fun() -> mnesia:write(Rec) end,
            {atomic, ok} = mnesia:transaction(F),
            Rec;
        true ->
            mnesia:write(Rec),
            Rec
    end.


lookup(Tab, Id) ->
    F = fun() -> mnesia:read(Tab, Id) end,
    case mnesia:transaction(F) of
    {atomic, [Result]} ->
        Result
    end.


maybe_lookup(Tab, Id) ->
    F = fun() -> mnesia:read(Tab, Id) end,
    case mnesia:transaction(F) of
    {atomic, [Result]} ->
        Result;
    {atomic, []} ->
        undefined
    end.


update_with(F, Tab, Id) when is_function(F, 1), Id =/= undefined ->
    TransF = fun() -> 
            [begin 
                NewRec = F(Rec),
                ok = mnesia:write(NewRec),
                NewRec
             end || Rec <- mnesia:read(Tab, Id)]
        end,
    case mnesia:transaction(TransF) of
    {atomic, Result} ->
        Result
    end.


read(Tab, Id) ->
    F = fun() -> mnesia:read(Tab, Id) end,
    case mnesia:transaction(F) of
    {atomic, Result} ->
        Result
    end.


remove(Table, Id) ->
    case mnesia:is_transaction() of
        false ->
            F = fun() -> mnesia:delete({Table, Id}), ok end,
            {atomic, ok} = mnesia:transaction(F),
            ok;
        true ->
            mnesia:delete({Table, Id}),
            ok
    end.



%% query list comprehenshions
select(Q)->
    %% to prevent against nested transactions
    %% to ensure it also works whether table
    %% is fragmented or not, we will use
    %% mnesia:activity/4

    case mnesia:is_transaction() of
        false ->
            F = fun(QH)-> qlc:e(QH) end,
            mnesia:activity(transaction,F,[Q],mnesia_frag);
        true -> qlc:e(Q)
    end.


transaction(F)->
    %% to prevent against nested transactions
    %% to ensure it also works whether table
    %% is fragmented or not, we will use
    %% mnesia:activity/4

    case mnesia:is_transaction() of
        false ->
            {atomic, X} = mnesia:transaction(F),
            X;
        true -> F()
    end.


select1(Q) ->
    case select(Q) of
        []  -> undefined;
        [X] -> X
    end.


match_object(Pattern) ->
    case mnesia:is_transaction() of
        false ->
            F = fun(P) -> mnesia:match_object(P) end,
            mnesia:activity(transaction,F,[Pattern],mnesia_frag);
        true ->
            mnesia:match_object(Pattern)
    end.


%% -----------------------------------------------------------------------
%% Private helpers
%% -----------------------------------------------------------------------

record_to_id(Rec) ->
    erlang:element(2, Rec).


