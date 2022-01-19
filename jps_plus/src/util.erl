%%%-------------------------------------------------------------------
%%% @author gz1407
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 1月 2022 11:09
%%%-------------------------------------------------------------------
-module(util).
-author("gz1407").

%% API
-export([for/3, for2/3]).


%% for循环
for(I, Max, _F) when I > Max -> ok;
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).


%% 两层循环 默认从0 开始   i 从 0 -> iMax  j 从 0 -> jMax
%% for循环
for2(IMax, JMax, F) ->
    for2(0, IMax, 0, JMax, F).

for2(I, IMax, _J, _JMax, _F) when I > IMax -> ok;
for2(CurI, IMax, J, JMax, F) when J > JMax ->

for2(CurI, IMax, J, JMax, F) ->
    F(Max);
for2(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).