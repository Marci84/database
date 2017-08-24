%%%-------------------------------------------------------------------
%% @doc database public API
%% @end
%%%-------------------------------------------------------------------

-module(database_app).

%%-behaviour(application).

%% Application callbacks
-export([create_database/3,create_database/4,add/2,select_with_key/2,show/0,help/0,find_pos/2,select/3,get_match/3]).

%%====================================================================
%% API
%%====================================================================

%%start(_StartType, _StartArgs) ->
%%    database_sup:start_link().

%%--------------------------------------------------------------------
%%stop(_State) ->
%%    ok.

%%====================================================================
%% Internal functions
%%====================================================================
create_database(Database_name,Fields,Key,Path)->
    case filelib:is_dir(Path) of
        true ->
            file:set_cwd(Path),
            create(Database_name,Fields,Key);
        false ->
            {error,invalid_path}
end.


create_database(Database_name,Fields,Key) when is_list(Fields) ->
    filelib:ensure_dir("/tmp/database/"),
    file:set_cwd("/tmp/database/"),
    create(Database_name,Fields,Key);
create_database(_,_,_)->
    {error,invalid_format}.

create(Database,Fields,Key)->
    Binary = io_lib:format("~p.~n",[list_to_tuple(Fields ++ [Key])]),
    file:write_file(Database,Binary).

add(Database,Fields) when is_list(Fields)->
    execute_if_database_exists(Database,check_if_key_not_exist(Database,Fields));
add(_,_) ->
    {error,fields_is_not_a_list}.

check_if_key_not_exist(Database,Fields)->
    Column_list = get_columns(Database),
    Key_position = get_keypos(Column_list),
    New_entry_key = lists:nth(Key_position,Fields),
    case select_with_key(Database,New_entry_key) of
        false ->
            add_fields(Database,Fields);
        _ ->
            {error,key_already_defined}
    end.

add_fields(Database,Fields)->
    Binary = io_lib:format("~p.~n",[list_to_tuple(Fields)]),
    file:write_file(Database,Binary,[append]).

select_with_key(Database,Filter)->
    execute_if_database_exists(Database,select_fields(Database,Filter)).

select_fields(Database,Filter)->
    Column_list = get_columns(Database),
    Key_pos = get_keypos(Column_list),
    lists:keyfind(Filter,Key_pos,get_database(Database)).

select(Database,Fields,Filter) when is_list(Fields), is_tuple(Filter)->
    execute_if_database_exists(Database,select_fields(Database,Fields,Filter));
select(_,_,_)->
    {error,invalid_format}.

select_fields(Database,Fields,Filter)->
    Column_list = get_columns(Database),
    {Key,Data} = Filter,
    Key_pos = find_pos(Column_list,Key),
    Column_filter = build_filter(Column_list,Fields),
%%    Values = lists:filter(get_match(Data,Key_pos,Column_list),get_database(Database)),
%%    Values = [List || {_,_,2007} = List <- get_database(Database)],
    Values = [List || List <- get_database(Database), Data == element(Key_pos,List)],
    list_to_tuple(build_result(Column_filter,Values)).

get_match(Data,Key_pos,Column_list)->
    Length = erlang:length(Column_list) - 1,
    Filter = lists:flatten(build_list(Data,Key_pos,Length,1)),
    list_to_tuple(Filter).

build_list(Data,Key_pos,Length,Count) when Length >= Count ->
    Element = case Count of
                  Key_pos ->
                      Data;
                  _ ->
                      "_"     
              end,
    [Element, build_list(Data,Key_pos,Length,Count+1)];
build_list(_,_,_,_)->
    [].

build_filter(_,[])->
    [];
build_filter(Column,[H|T])->
    lists:flatten([[find_pos(Column,H)] ++ build_filter(Column,T)]).

build_result(_,[])->
    [];
build_result(Column_filter,[H])->    
    build_element(Column_filter,H);
build_result(Column_filter,[H|T])-> 
    lists:flatten([build_element(Column_filter,H) ++ build_result(Column_filter,T)]).

build_element([],_)->
    [];
build_element([H|T],Values)->
    lists:flatten([[lists:nth(H,tuple_to_list(Values))] ++ build_element(T,Values)]).

show()->
    File_list = filelib:wildcard("*"),
    get_file_content(File_list).

get_file_content([])->
    [];
get_file_content([H|T])->
    {_,Content} = file:consult(H),
    lists:flatten([list_to_atom(H),Content]) ++ get_file_content(T).


find_pos(List,Name)->
    find_position(List,Name,1).

find_position([],_,Result)->
    Result;
find_position([H|T],Name,Result)->              
    case H of
        Name ->
            Result;
        _ ->
            find_position(T,Name,Result+1)
    end.            

execute_if_database_exists(Database,Action)->
    case filelib:is_file(Database) of
        true ->
            Action;
        false ->
            io:format("Invalid filename~n")
    end.

get_database(Database)->
    {_,File_content} = file:consult(Database),
    File_content.

get_columns(Database)->
    {_,File_content} = file:consult(Database),
    tuple_to_list(lists:nth(1,File_content)).

get_keypos(Column_list)->
    Key = lists:last(Column_list),
    find_pos(Column_list,Key).





help()->
    io:format("This is the help for database_app.~n" 
              "The following commands are valid:~n~n"
              "create_database/3 - database_app:create_database(movies,[year,name,actors],name).~n"
              "This will create a database called movies in the default directory \"/tmp/database/\" ~n~n"
              "create_database/4 - database_app:create_database(movies,[year,name,actors],name,\"/tmp/\").~n"
              "This will create a database called movies in the specified directory \"/tmp/\"~n~n"
              "add/2 - database_app:add(formula_one,[1,sebastian_vettel,2006]). ~n"
              "This will add a record to the specified database~n~n"
              "select/2 - database_app:select(\"formula_one\",2) ~n"
              "This will show the record with key 2 in databaes formula_one ~n~n"
              "show/0 ~n"
              "This will show all the databases with the records in it~n",[]).
