%%%-------------------------------------------------------------------
%%% @author chenyiming
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 一些定义
%%% @end
%%% Created : 19. 7月 2021 11:48
%%%-------------------------------------------------------------------
-ifndef(JPS_HRL).
-define(JPS_HRL, true).


-define(PRE_CREATE_MAP, true). %% 是否预处理跳点， 直接生成跳点
%% 类型定义========================================================================================================》start

%% 方向定义
-define(MOVING_DOWN,      (1 bsl 0)).  %% 向下  0001
-define(MOVING_RIGHT,     (1 bsl 1)).  %% 向右  0010
-define(MOVING_UP,        (1 bsl 2)).  %% 向上  0100
-define(MOVING_LEFT,      (1 bsl 3)).  %% 向左  1000

-type jump_4_dir() ::  ?MOVING_DOWN | ?MOVING_RIGHT | ?MOVING_UP | ?MOVING_LEFT.  %%跳点4个前进方向

%% 类型定义=========================================================================================================》end


-define(IF(Condition, T, F), (
    case Condition of
        true ->
            T;
        false ->
            F
    end
)).


-define(JUMP_POINT_ETS,                     jump_point).              %% 跳点ets
-define(DISTANT_JUMP_POINT_ETS,             distant_jump_point).      %% 距离跳点ets

-record(jump_point, {
    key = {0, 0}   %% 点的坐标
    ,point = 0     %% 跳点信息 0000  分别代表四个方向
}).


%%  -1 则代表这个方向没有跳点
%%  具体值表示距离
-record(dis_jump_point, {
    key = {0, 0}        %% 点的坐标
    ,down		= -1,   %% 默认-1 ，
    down_right	= -1,
    right		= -1,
    up_right	= -1,
    up			= -1,
    up_left		= -1,
    left		= -1,
    down_left	= -1
}).


-define(DATA_DIR, "./src/data").



-endif.