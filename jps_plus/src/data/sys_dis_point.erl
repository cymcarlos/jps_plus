%%%-------------------------------------------------------------------
%%% @author gz1407
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 1月 2022 15:16
%%%-------------------------------------------------------------------
-module(sys_dis_point).
-author("gz1407").

%% API
-export([]).

-include("jps.hrl").

get(x, y) ->
    #dis_jump_point{key = {~p, ~p}, down = ~p, down_right = ~p, right = ~p, up_right = ~p, up = ~p, up_left = ~p, left = ~p, down_left = ~p};