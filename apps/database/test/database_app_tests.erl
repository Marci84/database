-module(database_app_tests).
-include_lib("eunit/include/eunit.hrl").

create_database_test()->
    database_app:create_database(movies,[year,name,actors],name),
    ?assertEqual({ok,<<"{year,name,actors,name}.\n">>},file:read_file(movies)),
    file:delete(movies).

add_element_test()->
    database_app:create_database(movies,[year,name,actors],name),
    database_app:add(movies,[1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]]),
    ?assertEqual({ok,<<"{year,name,actors,name}.\n{1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]}.\n">>},file:read_file(movies)),
    file:delete(movies).


select_with_key_test()->
    database_app:create_database(movies,[year,name,actors],name),
    database_app:add(movies,[1972,the_godfather,al_pacino]),
    database_app:add(movies,[1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]]),
    database_app:add(movies,[1999,fight_club,[edward_norton,brad_pitt]]),
    ?assertEqual({1992,reservoir_dogs,
                  [harvey_keitel,tim_roth,michael_madsen]},database_app:select_with_key("movies",reservoir_dogs)),
    file:delete(movies).

show_test()->
    database_app:create_database(movies,[year,name,actors],name),
    database_app:add(movies,[1972,the_godfather,al_pacino]),
    database_app:add(movies,[1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]]),
    database_app:add(movies,[1999,fight_club,[edward_norton,brad_pitt]]),
    database_app:create_database(formula_one,[id,name,joining_year],id),
    database_app:add(formula_one,[1,sebastian_vettel,2006]),
    database_app:add(formula_one,[2,lewis_hamilton,2007]),
    database_app:add(formula_one,[3,valtteri_bottas,2013]),
    database_app:add(formula_one,[4,kimmi_raikkonen,2001]),    
    database_app:add(formula_one,[5,fernando_alonso,2001]),
    ?assertEqual([formula_one,{id,name,joining_year,id},{1,sebastian_vettel,2006},{2,lewis_hamilton,2007},{3,valtteri_bottas,2013},{4,kimmi_raikkonen,2001},{5,fernando_alonso,2001},movies,{year,name,actors,name},{1972,the_godfather,al_pacino},{1992,reservoir_dogs,[harvey_keitel,tim_roth,michael_madsen]},{1999,fight_club,[edward_norton,brad_pitt]}],database_app:show()),
    file:delete(movies),
    file:delete(formula_one).


find_pos_test()->
    ?assertEqual(3,database_app:find_position([a,b,c,d,e,f,g],c)).

find_pos2_test()->    
?assertEqual(5,database_app:find_position([a,b,c,d,e,e,e],e)).

select_test()->
    database_app:create_database(formula_one,[id,name,joining_year],id),
    database_app:add(formula_one,[1,sebastian_vettel,2006]),
    database_app:add(formula_one,[2,lewis_hamilton,2007]),
    database_app:add(formula_one,[3,valtteri_bottas,2013]),
    database_app:add(formula_one,[4,kimmi_raikkonen,2001]),    
    database_app:add(formula_one,[5,fernando_alonso,2001]),
    ?assertEqual({2,lewis_hamilton,2007},database_app:select("formula_one",[id,name,joining_year],{joining_year,2007})),
    file:delete(formula_one).

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
