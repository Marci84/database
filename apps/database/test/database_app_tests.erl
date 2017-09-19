-module(database_app_tests).
-include_lib("eunit/include/eunit.hrl").


database_test_()->
    {foreach,
     fun()->database_app:create_database(movies,[year,name,actors],name)end,
     fun(_)->file:delete(movies)end,
     [fun create_database_/0,fun add_element_/0,fun select_with_key_/0]
     }.

show_test_()->
    {setup,
     fun()-> database_app:create_database(movies,[year,name,actors],name),
           database_app:create_database(formula_one,[id,name,joining_year],id)end,
     fun(_)->file:delete(movies),file:delete(formula_one)end,
     fun show_/0
    }.
 
select_test_()->
    {setup,
     fun()->database_app:create_database(formula_one,[id,name,joining_year],id)end,
     fun(_)->file:delete(formula_one)end,
     fun select_/0
     }.


create_database_()->
    ?assertEqual({ok,<<"{year,name,actors,name}.\n">>},file:read_file(movies)).
   
add_element_()->
    database_app:add(movies,[1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]]),
    ?assertEqual({ok,<<"{year,name,actors,name}.\n{1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]}.\n">>},file:read_file(movies)).


select_with_key_()->
    database_app:add(movies,[1972,the_godfather,al_pacino]),
    database_app:add(movies,[1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]]),
    database_app:add(movies,[1999,fight_club,[edward_norton,brad_pitt]]),
    ?assertEqual({1992,reservoir_dogs,
                  [harvey_keitel,tim_roth,michael_madsen]},database_app:select_with_key("movies",reservoir_dogs)).

             
show_()->
    database_app:add(movies,[1972,the_godfather,al_pacino]),
    database_app:add(movies,[1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]]),
    database_app:add(movies,[1999,fight_club,[edward_norton,brad_pitt]]),
    database_app:add(formula_one,[1,sebastian_vettel,2006]),
    database_app:add(formula_one,[2,lewis_hamilton,2007]),
    database_app:add(formula_one,[3,valtteri_bottas,2013]),
    database_app:add(formula_one,[4,kimmi_raikkonen,2001]),    
    database_app:add(formula_one,[5,fernando_alonso,2001]),
    ?assertEqual([[formula_one,{id,name,joining_year,id},{1,sebastian_vettel,2006},{2,lewis_hamilton,2007},{3,valtteri_bottas,2013},{4,kimmi_raikkonen,2001},{5,fernando_alonso,2001}],[movies,{year,name,actors,name},{1972,the_godfather,al_pacino},{1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]},{1999,fight_club,[edward_norton,brad_pitt]}]],database_app:show()).

select_()->
    database_app:add(formula_one,[1,sebastian_vettel,2006]),
    database_app:add(formula_one,[2,lewis_hamilton,2007]),
    database_app:add(formula_one,[3,valtteri_bottas,2013]),
    database_app:add(formula_one,[4,kimmi_raikkonen,2001]),    
    database_app:add(formula_one,[5,fernando_alonso,2001]),
    ?assertEqual({2,lewis_hamilton,2007},database_app:select("formula_one",[id,name,joining_year],{joining_year,2007})).

select_with_filter_test()->
    database_app:create_database(formula_one,[id,name,joining_year],id),
    database_app:add(formula_one,[1,sebastian_vettel,2006]),
    database_app:add(formula_one,[2,lewis_hamilton,2007]),
    database_app:add(formula_one,[3,valtteri_bottas,2013]),
    database_app:add(formula_one,[4,kimmi_raikkonen,2001]),    
    database_app:add(formula_one,[5,fernando_alonso,2001]),
    ?assertEqual({lewis_hamilton},database_app:select("formula_one",[name],{joining_year,2007})).

select_with_filter_multiple_result_test()->
    database_app:create_database(formula_one,[id,name,joining_year],id),
    database_app:add(formula_one,[1,sebastian_vettel,2006]),
    database_app:add(formula_one,[2,lewis_hamilton,2007]),
    database_app:add(formula_one,[3,valtteri_bottas,2013]),
    database_app:add(formula_one,[4,kimmi_raikkonen,2001]),    
    database_app:add(formula_one,[5,fernando_alonso,2001]),
    ?assertEqual({4,2001,5,2001},database_app:select("formula_one",[id,joining_year],{joining_year,2001})).


find_pos_test()->
    ?assertEqual(3,database_app:find_position([a,b,c,d,e,f,g],c)).

find_pos2_test()->    
    ?assertEqual(5,database_app:find_position([a,b,c,d,e,e,e],e)).
