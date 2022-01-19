%%%-------------------------------------------------------------------
%%% @author gz1407
%%% @copyright (C) 2022, <COMPANY>
%%% @doc  跳点预处理， 第一版先放在ets 中， 后续考虑生成erl文件， 程序启动时直接加载跳点到erlang节点中,
%%%       注意点的下标是0开始的。
%%%       一般认为(x, y)排列为
%%%%              (0, 0), (1, 0), (2, 0)
%%%%              (0, 1), (1, 1), (2, 1)
%%%%              (0, 2), (1, 2), (2, 2)
%%%       优化方向
%%%       1. 判断是否是障碍点事线性时间 较慢
%%% @end
%%% Created : 17. 1月 2022 17:48
%%%-------------------------------------------------------------------
-module(pre_calc_map).
-author("gz1407").



-include("jps.hrl").
%% API
-export([calculate_map/4, map_gen_test/0]).



%% 测试例子
map_gen_test() ->
    BlockList = [{2, 0}, {6, 0},
                {6, 1},
                {1, 2}, {2, 2}, {6, 2}, {7, 2},
                {2, 3},
                {2, 4}, {6, 4}
    ],
    %% 为什么用两个?， 是不想多个地方用not
    ValidFun = fun({X,Y}) -> not lists:member({X,Y}, BlockList) end, %% 判断可行区域
    IsWallFun = fun({X,Y}) ->  lists:member({X,Y}, BlockList) end,   %% 判断障碍区
    %%  5 * 9 的地图
    RowMax = 4,
    ColMax = 8,
    calculate_map(IsWallFun, ValidFun, RowMax, ColMax).


%% @doc 预计算跳点
%% @end
-spec calculate_map(IsWallFun, ValidFun, RowMax, ColMax) -> ok when
    IsWallFun::any(),    %% 判断是否障碍的点的方法， 如果是障碍或则边界的点， 则返回true 否则 false
    ValidFun::any(),     %% 判断是否合法的点的方法， 如果是合法的点， 则返回true 否则 false
    RowMax  ::integer(), %% 行最大值（注意不是长度） 相当于 y
    ColMax  ::integer(). %% 列最大值（注意不是长度）       x
calculate_map(IsWallFun, ValidFun, RowMax, ColMax) ->
    if
        ?PRE_CREATE_MAP == true ->
        %% 初始化跳点ets
        ets:new(?JUMP_POINT_ETS, [named_table, set, public, {keypos, #jump_point.key}, {read_concurrency, true}]),
        RowList   = lists:seq(0, RowMax),
        ColList   = lists:seq(0, ColMax),
        %% 计算跳点
        calculate_jump_point_map(RowList, ColList, ValidFun, IsWallFun),
        %% 初始化跳点距离ets
        ets:new(?DISTANT_JUMP_POINT_ETS, [named_table, set, public, {keypos, #dis_jump_point.key}, {read_concurrency, true}]),
        %% 计算跳点距离
        calculate_distant_jump_point_map(RowList, ColList, ValidFun, IsWallFun);
        true ->
            skip
    end.

%% @doc 判断这个点在(RowDir, ColDir)这个方向 是否是跳点
%%      eg 假设 ColDir = 1  RowDir = 0， 其中@是障碍点 其他都是可行区 ,
%%      x为当前点(R, C), 则p为x的父节点， 如下图
%%
%%          0   0   0   0   0   0
%%          0   @1  n1  0   0   0          ====> 情况1 n1 为x的强迫邻居 >>
%%          0   p   x   0   0   0                                       >>>> 满足其中之一则x为跳点
%%          0   @2  n2  0   0   0          ====> 情况2 n2 为x的强迫邻居 >>
%%          0   0   0   0   0   0
%% @end
-spec is_jump_point(R, C, RowDir, ColDir, ValidFun, IsWallFun) -> boolean() when
    R::integer(),       % 当前行坐标
    C::integer(),       % 当前列坐标
    RowDir::integer(),  % 当前行方向
    ColDir::integer(),  % 当前列方向
    ValidFun::any(),    % 判断是否可行点方法
    IsWallFun::any().   % 判断是否障碍点方法
is_jump_point(R, C, RowDir, ColDir, ValidFun, IsWallFun) ->
    ValidFun({R - RowDir, C - ColDir})  %%父节点是可行区域(非必须， 这里按澳大利亚教授的写法也写上了)
    andalso
    (
        (ValidFun({R + ColDir, C + RowDir}) andalso  IsWallFun({R - RowDir + ColDir, C - ColDir + RowDir}))  % 情况1
        orelse
        (ValidFun({R - ColDir, C - RowDir}) andalso  IsWallFun({R - RowDir - ColDir, C - ColDir - RowDir}))  % 情况2
    ).



%% @doc  计算跳点， 插入到ets中
%% @end
-spec calculate_jump_point_map(LRow, LCol, ValidFun, IsWallFun) -> ok when
    LRow::[integer()],  % 行坐标列表
    LCol::[integer()],  % 列坐标列表
    ValidFun::any(),    % 判断是否可行点方法
    IsWallFun::any().   % 判断是否障碍点方法
calculate_jump_point_map(LRow, LCol, ValidFun, IsWallFun) ->
    [ [ begin
            case ValidFun({R, C}) of
                true -> %% 可行区域
                    Rec1 = #jump_point{key = {R, C}},
                    
                    %% 下方向
                    case is_jump_point(R, C, 1, 0, ValidFun, IsWallFun) of
                        true ->
                            Rec2 = Rec1#jump_point{point = Rec1#jump_point.point  bor ?MOVING_DOWN};
                        _ ->
                            Rec2 = Rec1
                    end,
                    
                    %% 上方向
                    case is_jump_point(R, C, -1, 0, ValidFun, IsWallFun) of
                        true ->
                            Rec3 = Rec2#jump_point{point = Rec2#jump_point.point bor ?MOVING_UP};
                        _ ->
                            Rec3 = Rec2
                    end,
                    
                    %% 右方向
                    case is_jump_point(R, C, 0, 1, ValidFun, IsWallFun) of
                        true ->
                            Rec4 = Rec3#jump_point{point = Rec3#jump_point.point bor ?MOVING_RIGHT};
                        _ ->
                            Rec4 = Rec3
                    end,
                    
                    %% 左方向
                    case is_jump_point(R, C, 0, -1, ValidFun, IsWallFun) of
                        true ->
                            Rec5 = Rec4#jump_point{point = Rec4#jump_point.point bor ?MOVING_LEFT};
                        _ ->
                            Rec5 = Rec4
                    end,
                    
                    %% 插入ets表
                    ets:insert(?JUMP_POINT_ETS, Rec5);
                _ ->
                    skip
            end
        end || C <- LCol] || R <- LRow].

%% @doc  通过ets来判断是否跳点， 前提是计算了跳点ets(calculate_jump_point_map要执行了才有效)
%% @end
-spec is_jump_point_by_ets(Row, Col, Dir) -> boolean() when
    Row ::integer() , % 行坐标
    Col ::integer() , % 列坐标
    Dir ::jump_4_dir().
is_jump_point_by_ets(Row, Col, Dir) ->
    case ets:lookup(?JUMP_POINT_ETS, {Row, Col}) of
        [#jump_point{point = Point}] ->
            Res = Point band  Dir,
            ?IF(Res > 0, true, false);
        _ ->
            false
    end.

%% @doc  获取跳点距离记录
%% @end
-spec get_dis_jump_rec(integer(), integer()) ->  #dis_jump_point{}.
get_dis_jump_rec(R, C) ->
    case ets:lookup(?DISTANT_JUMP_POINT_ETS, {R, C}) of
        [Rec] ->
            Rec;
        _ ->
            #dis_jump_point{key = {R, C}}
    end.



calculate_distant_jump_point_map_left_and_right([], _LCol, _ValidFun, _IsWallFun) ->
    ok;
calculate_distant_jump_point_map_left_and_right([Row | T], LCol, _ValidFun, IsWallFun) ->
    InitCountMoving = -1,   %% 初始化跳点的距离，-1 则是没有跳点
    InitJumpPointLastSeen = false, %% 初始化跳点可视标识，默认看不到
    %%==================================================================================
    %% 左边 从0 --->  max, 从左到右遍历,计算左边的跳点的距离
    %%==================================================================================
    FunLeft =
        fun(Col, {CountMovingLeft, JumpPointLastSeen}) ->
            DisRec = get_dis_jump_rec(Row, Col),
            case IsWallFun({Row, Col}) of
                true-> %% 如果是障碍点再次初始化
                    DisRecNew = DisRec#dis_jump_point{left = 0},
                    Acc = {-1, false};
                _ -> %% 是可行点
                    CountMovingLeftNew = CountMovingLeft + 1, %% 移动距离 + 1
                    if
                        JumpPointLastSeen == true -> %% 如果在这个方向可以看到跳点， 则跳点距离为CountMovingLeftNew
                            DisRecNew = DisRec#dis_jump_point{left = CountMovingLeftNew};
                        true -> %% Wall last seen 看到的是墙
                            DisRecNew = DisRec#dis_jump_point{left = -CountMovingLeftNew}
                    end,
                    case is_jump_point_by_ets(Row, Col, ?MOVING_LEFT) of %%如果对于左边方向是跳点， 则更新左边可以看到跳点
                        true ->
                            Acc = {0, true};
                        _ ->
                            Acc = {CountMovingLeftNew, JumpPointLastSeen}  % 保持JumpPointLastSeen标识， 距离 + 1
                    end
            end,
            ets:insert(?DISTANT_JUMP_POINT_ETS, DisRecNew),
            Acc
        end,
    lists:foldl(FunLeft, {InitCountMoving, InitJumpPointLastSeen}, LCol),
    
    %%==================================================================================
    %% 右边  和左边的同理，  从0 <---  max, 从右到左遍历,计算右边的跳点的距离
    %%==================================================================================
    FunRight =
        fun(Col, {CountMovingRight, JumpPointLastSeen}) ->
            DisRec = get_dis_jump_rec(Row, Col),
            case IsWallFun({Row, Col}) of
                true-> %% 如果是障碍点再次初始化
                    DisRecNew = DisRec#dis_jump_point{right = 0},
                    Acc = {-1, false};
                _ -> %% 是可行点
                    CountMovingRightNew = CountMovingRight + 1, %% 移动距离 + 1
                    if
                        JumpPointLastSeen == true ->
                            DisRecNew = DisRec#dis_jump_point{right = CountMovingRightNew};
                        true -> %% Wall last seen 看到的是墙
                            DisRecNew = DisRec#dis_jump_point{right = -CountMovingRightNew}
                    end,
                    case is_jump_point_by_ets(Row, Col, ?MOVING_RIGHT) of
                        true ->
                            Acc = {0, true};
                        _ ->
                            Acc = {CountMovingRightNew, JumpPointLastSeen}
                    end
            end,
            ets:insert(?DISTANT_JUMP_POINT_ETS, DisRecNew),
            Acc
        end,
    lists:foldl(FunRight, {InitCountMoving, InitJumpPointLastSeen}, lists:reverse(LCol)),
    calculate_distant_jump_point_map_left_and_right(T, LCol, _ValidFun, IsWallFun).



calculate_distant_jump_point_map_up_and_down(_LRow, [], _ValidFun, _IsWallFun) ->
    ok;
calculate_distant_jump_point_map_up_and_down(LRow, [Col | T], _ValidFun, IsWallFun) ->
    InitCountMoving = -1,   %% 初始化跳点的距离，-1 则是没有跳点
    InitJumpPointLastSeen = false, %% 初始化跳点可视标识，默认看不到
    FunUp =
        fun(Row, {CountMoving, JumpPointLastSeen}) ->
            DisRec = get_dis_jump_rec(Row, Col),
            case IsWallFun({Row, Col}) of
                true-> %% 如果是障碍点再次初始化
                    DisRecNew = DisRec#dis_jump_point{up = 0},
                    Acc = {-1, false};
                _ -> %% 是可行点
                    CountMovingNew = CountMoving + 1, %% 移动距离 + 1
                    if
                        JumpPointLastSeen == true -> %% 如果在这个方向可以看到跳点， 则跳点距离为CountMovingNew
                            DisRecNew = DisRec#dis_jump_point{up = CountMovingNew};
                        true -> %% Wall last seen 看到的是墙
                            DisRecNew = DisRec#dis_jump_point{up = -CountMovingNew}
                    end,
                    case is_jump_point_by_ets(Row, Col, ?MOVING_UP) of
                        true ->
                            Acc = {0, true};
                        _ ->
                            Acc = {CountMovingNew, JumpPointLastSeen}
                    end
            end,
            ets:insert(?DISTANT_JUMP_POINT_ETS, DisRecNew),
            Acc
        end,
    lists:foldl(FunUp, {InitCountMoving, InitJumpPointLastSeen}, LRow),
    
    FunDown =
        fun(Row, {CountMoving, JumpPointLastSeen}) ->
            DisRec = get_dis_jump_rec(Row, Col),
            case IsWallFun({Row, Col}) of
                true-> %% 如果是障碍点再次初始化
                    DisRecNew = DisRec#dis_jump_point{down = 0},
                    Acc = {-1, false};
                _ -> %% 是可行点
                    CountMovingNew = CountMoving + 1, %% 移动距离 + 1
                    if
                        JumpPointLastSeen == true -> %% 如果在这个方向可以看到跳点， 则跳点距离为CountMovingNew
                            DisRecNew = DisRec#dis_jump_point{down = CountMovingNew};
                        true -> %% Wall last seen 看到的是墙
                            DisRecNew = DisRec#dis_jump_point{down = -CountMovingNew}
                    end,
                    case is_jump_point_by_ets(Row, Col, ?MOVING_DOWN) of
                        true ->
                            Acc = {0, true};
                        _ ->
                            Acc = {CountMovingNew, JumpPointLastSeen}
                    end
            end,
            ets:insert(?DISTANT_JUMP_POINT_ETS, DisRecNew),
            Acc
        end,
    lists:foldl(FunDown, {InitCountMoving, InitJumpPointLastSeen}, lists:reverse(LRow)),
    calculate_distant_jump_point_map_up_and_down(LRow, T, _ValidFun, IsWallFun).



calculate_distant_jump_point_map_leftup_and_rightup(LRow, LCol, ValidFun, IsWallFun, _MaxRow, MaxCol) ->
    [[ begin
           Rec = get_dis_jump_rec(Row, Col),
           case ValidFun({Row, Col}) of
               true ->
                   %%======================================== 左上方向==============================================
                   case
                       Row == 0 orelse                      % 第一行
                       Col == 0 orelse                      % 第一列
                       IsWallFun({Row - 1, Col}) orelse     % 上面是障碍
                       IsWallFun({Row, Col - 1}) orelse     % 左边是障碍
                       IsWallFun({Row - 1, Col-1})          % 左上是障碍
                   of
                       true ->  %% 这个条路被封了，没有跳点
                           Rec1 = Rec#dis_jump_point{up_left = 0};
                       _ -> %% 这条路可行
                           RecUpLeft = get_dis_jump_rec(Row - 1, Col - 1),
                           if
                               %% 左上的点A 的上方向有跳点 或者 左方向有跳点===》
                               %% 则认为当前点（Row, Col）左上方向有一个跳点(就是A)， 且距离为1
                               RecUpLeft#dis_jump_point.up > 0 orelse  RecUpLeft#dis_jump_point.left > 0 ->
                                   Rec1 = Rec#dis_jump_point{up_left = 1};
                               true ->
                                   if
                                       RecUpLeft#dis_jump_point.up_left > 0 -> %如果左上点A 的左上方向有跳点， 则在此基础上 + 1
                                           Rec1 = Rec#dis_jump_point{up_left = RecUpLeft#dis_jump_point.up_left  + 1};
                                       true -> %% 否则没有跳点
                                           Rec1 = Rec#dis_jump_point{up_left = RecUpLeft#dis_jump_point.up_left - 1}
                                   end
                           end
                   end,
                   
                   %% 同理
                   %%======================================== 右上方向==============================================
                   case
                       Row == 0 orelse
                       Col == MaxCol orelse
                       IsWallFun({Row - 1, Col}) orelse
                       IsWallFun({Row, Col + 1}) orelse
                       IsWallFun({Row - 1, Col + 1})
                   of
                       true ->
                           Rec2 = Rec1#dis_jump_point{up_right = 0};
                       _ ->
                           RecUpRight = get_dis_jump_rec(Row - 1, Col + 1),
                           if
                               RecUpRight#dis_jump_point.up > 0 orelse  RecUpRight#dis_jump_point.right > 0 ->
                                   Rec2 = Rec1#dis_jump_point{up_right = 1};
                               true ->
                                   if
                                       RecUpRight#dis_jump_point.up_right > 0 ->
                                           Rec2 = Rec1#dis_jump_point{up_right = RecUpRight#dis_jump_point.up_right  + 1};
                                       true -> %% 否则没有跳点
                                           Rec2 = Rec1#dis_jump_point{up_right = RecUpRight#dis_jump_point.up_right - 1}
                                   end
                           end
                   end,
                   
                   ets:insert(?DISTANT_JUMP_POINT_ETS, Rec2);
               _ ->
                   skip
           end
       end || Col <- LCol] || Row <- LRow].


%% 左下的右下的计算
calculate_distant_jump_point_map_leftdown_and_rightdown(LRow, LCol, ValidFun, IsWallFun, MaxRow, MaxCol) ->
    [[ begin
           Rec = get_dis_jump_rec(Row, Col),
           case ValidFun({Row, Col}) of
               true ->
                   %%======================================== 左下方向==============================================
                   case
                       Row == MaxRow orelse
                       Col == 0 orelse
                       IsWallFun({Row + 1, Col}) orelse
                       IsWallFun({Row, Col - 1}) orelse
                       IsWallFun({Row + 1, Col - 1})
                   of
                       true ->
                           Rec1 = Rec#dis_jump_point{down_left = 0};
                       _ ->
                           RecDownLeft = get_dis_jump_rec(Row + 1, Col - 1),
                           if
                               RecDownLeft#dis_jump_point.down > 0 orelse  RecDownLeft#dis_jump_point.left > 0 ->
                                   Rec1 = Rec#dis_jump_point{down_left = 1};
                               true ->
                                   if
                                       RecDownLeft#dis_jump_point.down_left > 0 ->
                                           Rec1 = Rec#dis_jump_point{down_left = RecDownLeft#dis_jump_point.down_left  + 1};
                                       true ->
                                           Rec1 = Rec#dis_jump_point{down_left = RecDownLeft#dis_jump_point.down_left - 1}
                                   end
                           end
                   end,
    
    
                   %%======================================== 右下方向==============================================
                   case
                           Row == MaxRow orelse
                           Col == MaxCol orelse
                           IsWallFun({Row + 1, Col}) orelse
                           IsWallFun({Row, Col + 1}) orelse
                           IsWallFun({Row + 1, Col + 1})
                   of
                       true ->
                           Rec2 = Rec1#dis_jump_point{down_right = 0};
                       _ ->
                           RecDownRight = get_dis_jump_rec(Row + 1, Col + 1),
                           if
                               RecDownRight#dis_jump_point.down > 0 orelse  RecDownRight#dis_jump_point.right > 0 ->
                                   Rec2 = Rec1#dis_jump_point{down_right = 1};
                               true ->
                                   if
                                       RecDownRight#dis_jump_point.down_right > 0 ->
                                           Rec2 = Rec1#dis_jump_point{down_right = RecDownRight#dis_jump_point.down_right  + 1};
                                       true -> %%
                                           Rec2 = Rec1#dis_jump_point{down_right = RecDownRight#dis_jump_point.down_right - 1}
                                   end
                           end
                   end,
                
                   ets:insert(?DISTANT_JUMP_POINT_ETS, Rec2);
               _ ->
                   skip
           end
       end || Col <- LCol] || Row <- LRow].

%% @doc  计算跳点距离
%% @end
-spec calculate_distant_jump_point_map(LRow, LCol, ValidFun, IsWallFun) -> ok when
    LRow::[integer()],  % 行坐标列表
    LCol::[integer()],  % 列坐标列表
    ValidFun::any(),    % 判断是否可行点方法
    IsWallFun::any().   % 判断是否障碍点方法
calculate_distant_jump_point_map(LRow, LCol, ValidFun, IsWallFun) ->
    %%左右的计算
    calculate_distant_jump_point_map_left_and_right(LRow, LCol, ValidFun, IsWallFun),
    %%上下的计算 类似左右
    calculate_distant_jump_point_map_up_and_down(LRow, LCol, ValidFun, IsWallFun),
    
    RowMax = length(LRow) - 1,
    ColMax = length(LCol) - 1,
    %%左上的右上的计算
    calculate_distant_jump_point_map_leftup_and_rightup(LRow, LCol, ValidFun, IsWallFun, RowMax, ColMax),
    %% 左下的右下的计算   和左上的右上的计算同理
    calculate_distant_jump_point_map_leftdown_and_rightdown(lists:reverse(LRow), LCol, ValidFun, IsWallFun, RowMax, ColMax).
