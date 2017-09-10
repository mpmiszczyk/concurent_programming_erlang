-module(super).
-export([super/0]).


%% KILLING PROCESSES STARTED FROM SHELL
%%
%% This is first part of exercise.  We start and register `echo`
%% process, and then start `talk` worker.
%%
%% Killing worker just stops messages being send.  `echo` is still
%% alive and registered.  You also could have few `talk` workers,
%% killing one won't affect others.
%%
%% On the other hand killing `echo` process in most cases will kill
%% both `echo` and `talk`.  This is not due to linking, but rather to
%% fact that killing registered process (`echo`) will cause it's name
%% to unregister, and `talk` worker who will try to send message to
%% unregistered atom will fail.  But not always.
%%
%% `talk` worker most  of the time sleeps, but there  is small portion
%% of time between  sending message and receiving  response that could
%% cause problems.   If we  kill `echo` at  this moment  `talk` worker
%% will block on `receive` and hang there infinitely.
%%
%% To test this behavior one only need to move `timer:sleep(500)`
%% from inside `receive` to somewhere between `receive` and `!`.
%%


super() ->
    process_flag(trap_exit, true),
    E = spawn_link(echo,listener,[]),
    register(echo,E),
    io:format("echo spawned.~n"),
    T = spawn_link(talk,worker,[]),
    %% not sure what for we register `talk` since `echo` is sending
    %% messages back not trough atom, but trough Pid provided with
    %% message
    register(talk,T),
    io:format("worked spawned as Pid ~w.~n",[whereis(talk)]),
    loop(E,T).



%% This implementation was provided inside zip file
loop(E,T) ->
     receive
        {'EXIT', T, _} ->
            NewT = spawn_link(talk,worker,[]),
            register(talk,NewT),
            io:format("worked re-spawned as Pid ~w.~n",[whereis(talk)]),
            loop(E,NewT);
         {'EXIT', E, _} ->
            timer:sleep(1000),
            NewE = spawn_link(echo,listener,[]),
            register(echo,NewE),
            io:format("echo re-spawned.~n"),
            loop(NewE,T)
    end.

loop2(E,T) ->
     receive
         {'EXIT', T, _} ->
            loop(E,restart(talk));
         {'EXIT', E, _} ->
            %% we kill worker just in case of "race condition"
            %% described at the beginning of file
            exit(E, kill),
            loop(restart(echo), restart(talk))
    end.

restart(Module) ->
  NewProcess = spawn_link(talk,worker,[]),
  register(Module,NewProcess),
  io:format("~p re-spawned as Pid ~w.~n",[Module, whereis(Module)]),
  NewProcess.
