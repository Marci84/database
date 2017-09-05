%%%-------------------------------------------------------------------
%% @doc database public API
%% @end
%%%-------------------------------------------------------------------

-module(database_app).

%%-behaviour(application).

%% Application callbacks
-export([create_database/3,create_database/4,add/2,select_with_key/2,show/0,help/0,find_position/2,select/3]).

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
            invalid_path
end.


create_database(Database_name,Fields,Key) when is_list(Fields) ->
    filelib:ensure_dir("/tmp/database/"),
    file:set_cwd("/tmp/database/"),
    create(Database_name,Fields,Key);
create_database(_,_,_)->
    invalid_format.

create(Database,Fields,Key)->
    Binary = io_lib:format("~p.~n",[list_to_tuple(Fields ++ [Key])]),
    file:write_file(Database,Binary).

add(Database,Fields) when is_list(Fields)->
    Action = fun()-> check_if_key_not_exist(Database,Fields) end,
    execute_if_database_exists(Database,Action);
add(_,_) ->
    fields_is_not_a_list.

check_if_key_not_exist(Database,Fields)->
    Column_list = get_columns(Database),
    Key_position = get_keypos(Column_list),
    New_entry_key = lists:nth(Key_position,Fields),
    case select_with_key(Database,New_entry_key) of
        false ->
            add_fields(Database,Fields);
        _ ->
            key_already_defined
    end.

add_fields(Database,Fields)->
    Binary = io_lib:format("~p.~n",[list_to_tuple(Fields)]),
    file:write_file(Database,Binary,[append]).

select_with_key(Database,Filter)->
    Action = fun()-> select_fields(Database,Filter) end,
    execute_if_database_exists(Database,Action).

select_fields(Database,Filter)->
    Column_list = get_columns(Database),
    Key_pos = get_keypos(Column_list),
    lists:keyfind(Filter,Key_pos,get_database(Database)).

select(Database,Fields,Filter) when is_list(Fields), is_tuple(Filter)->
    Action = fun()-> select_fields(Database,Fields,Filter) end,
    execute_if_database_exists(Database,Action);
select(_,_,_)->
    invalid_format.

select_fields(Database,Fields,Filter)->
    Column_list = get_columns(Database),
    {Key,Data} = Filter,
    Key_pos = find_position(Column_list,Key),
    Column_filter = build_filter(Column_list,Fields),
    Values = [List || List <- get_database(Database), Data == element(Key_pos,List)],
    list_to_tuple(build_result(Column_filter,Values)).

build_filter(_,[])->
    [];
build_filter(Column,[H|T])->
    [find_position(Column,H)|build_filter(Column,T)].

build_result(_,[])->
    [];
build_result(Column_filter,[H])->    
    build_element(Column_filter,H);
build_result(Column_filter,[H|T])-> 
    lists:flatten([build_element(Column_filter,H)|build_result(Column_filter,T)]).

build_element([],_)->
    [];
build_element([H|T],Values)->
    [lists:nth(H,tuple_to_list(Values))|build_element(T,Values)].

show()->
    File_list = filelib:wildcard("*"),
    get_file_content(File_list).

get_file_content([])->
    [];
get_file_content([H|T])->
    {_,Content} = file:consult(H),
    lists:flatten([list_to_atom(H),Content]) ++ get_file_content(T).


find_position(List,Name)->
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
            Action();
        false ->
            invalid_filename
    end.

get_database(Database)->
    {_,File_content} = file:consult(Database),
    File_content.

get_columns(Database)->
    {_,File_content} = file:consult(Database),
    tuple_to_list(lists:nth(1,File_content)).

get_keypos(Column_list)->
    Key = lists:last(Column_list),
    find_position(Column_list,Key).

help()->
    io:format("This is the help for database_app.~n" 
              "The following commands are valid:~n~n"
              "create_database/3 - database_app:create_database(movies,[year,name,actors],name).~n"
              "This will create a database called movies in the default directory \"/tmp/database/\" ~n~n"
              "create_database/4 - database_app:create_database(movies,[year,name,actors],name,\"/tmp/\").~n"
              "This will create a database called movies in the specified directory \"/tmp/\"~n~n"
              "add/2 - database_app:add(movies,[1972,the_godfather,al_pacino]). ~n"
              "This will add a record to the specified database~n~n"
              "select_with_key/2 - database_app:select(\"movies\",1972) ~n"
              "This will show the record with key 1972 in databaes movies ~n~n"
              "database_app:select(movies,[year,actors],{name,the_godfather}). ~n"
              "This will show the record with key the_godfather but with filtered columns - year,actors ~n"

              "show/0 ~n"
              "This will show all the databases with the records in it~n",[]).
