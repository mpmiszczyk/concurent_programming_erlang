%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency2).
-export([start/0,
         allocate/0,
         deallocate/1,
         stop/0
        ]).
-export([init/0,
         loop/1]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(?MODULE,
             spawn(?MODULE, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  ?MODULE:loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      ?MODULE:loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      ?MODULE:loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() ->
    %% clear_frequency_messages(),  % could be used here
    ?MODULE ! {request, self(), allocate},
    receive
      {reply, Frequency} ->
        Frequency
    %% Now we will add timeouts to our calls,
    %%
    %% Throwing an exception would not be my first choice.  I would
    %% prefer to return atom `timeout` on error, and `{ok, Frequency}`
    %% upon success, but that would be considered a change in
    %% interface.  And returning just `timeout` or `Frequency` would
    %% not be easy detectable, client would assign atom to variable
    %% and not realized that it's nor `Frequency` until much later.
    %% So, in my opinion, throwing an exception is only solution in
    %% this case.
    after 1000 ->
        throw(timeout)
    end.

deallocate(Freq) ->
    ?MODULE ! {request, self(), {deallocate, Freq}},
    receive
      {reply, Reply} ->
        Reply
    after 1000 ->
        throw(timeout)
    end.

stop() ->
    ?MODULE ! {request, self(), stop},
    receive
      {reply, Reply} ->
        Reply
    after 1000 ->
        throw(timeout)
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.


%% Clear all messages from mailbox, and then continue
clear() ->
  receive
    _ ->
      clear()
  after 0 ->
      ok
  end.

%% This function will clear only messages in scheme of replays from
%% frequency server.  It makes little more usable, since any process
%% could be receiving messages from many sources and in many schemes.
%%
%% While this approach is little more correct we still do not have any
%% guaranties that some other process will not use same scheme, and we
%% will unwillingly throw away it's messages.
%%
%% We could change `reply` atom to something more unique, like
%% `frequency2_reply`.  It would not change interface, just "internal"
%% communication (still hidden behind interface functions).
%%
%% It could be used just at the beginning of interface functions, and
%% it would allow to clear all old lingering messages even before we
%% ask for new one.
clear_frequency_messages() ->
  receive
    {reply, _} -> % again, `reply` could be changed to something
      clear_frequency_messages()
  after 0 ->
      ok
  end.


%% Third option would be clearing mailbox by unique message.
%%
%% As soon as we notice timeout we could leave lingering function
%% which would try to clear one specific message from mailbox.  It
%% would mean that we need to make each message unique, so it could
%% be recognized in later time.
%%
%% It would require changes both inside interface functions and in
%% `loop` since unique identifier would have to be send there and
%% back.

%% Don't forget to export this function
clear_later(Ref) ->
  receive
    {reply, _, Ref} ->
      cleared
  after 0 ->
      ok
  end.


deallocate2(Freq) ->
    Ref = make_ref(), % this is unique identifier
    ?MODULE ! {request, self(), Ref, {deallocate, Freq}}, % we add `make_ref
    receive
      {reply, Reply, Ref} ->
        Reply
    after 1000 ->
        timer:apply_after(10*1000, ?MODULE, clear_later, [Ref]),
        throw(timeout)
    end.

%% In the end I don't believe that you should just clear those
%% messages.  It doesn't solve problem, it hides it.  If we remove
%% message allocating frequency from clients mailbox we won't have any
%% way do deallocate it, it will be "lost" almost for ever.  Things
%% like messages hanging in mailbox could be considered just a memory
%% leaks, and if you build a system you have to monitor for those
%% anyway (which is not so hard with Erlang).  If you find any big
%% enough that it causes you problems than you should try to find a
%% solution, and don't try to anticipate problems when you don't have
%% them yet, it will just create more of them.  In my last example I
%% create lingering `clear_later` call which could put more strain on
%% system than one not cleared message in mailbox.  At least that's my
%% point of view at this time.
