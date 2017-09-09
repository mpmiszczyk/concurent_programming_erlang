-module(frequency).

-export([
         start/1,
         allocate/1,
         deallocate/2
        ]).

-export([
         loop/1,
         init/1
        ]).



start(ListOfFreqs) ->
  spawn(?MODULE, init, [ListOfFreqs]).

allocate(Freqs) ->
  Freqs ! {allocate, self(), Ref = make_ref()},
  receive
    {ok, Freq, Freqs, Ref} ->
      {ok, Freq, Freqs}
  end.

deallocate(Freq, Freqs) ->
  Freqs ! {deallocate, Freq}.



init(ListOfFreqs) ->
  ?MODULE:loop({{ListOfFreqs, []}, self()}).

loop({Frequencies, Server}) ->
  receive
    {allocate, Client, Ref} ->
      {ok, Allocated, NewFrequencies} = handle_allocate(Frequencies),
      Client ! {ok, Allocated, Server, Ref},
      ?MODULE:loop({NewFrequencies, Server});
    {deallocate, Frequency} ->
      {ok, NewFreqs} = handle_deallocate(Frequency, Frequencies),
      ?MODULE:loop({NewFreqs, Server})
  end.

handle_allocate({ [FirstFree|RestOfFree],
                  AllocatedFreqs} ) ->
  {ok,
   FirstFree,
  {RestOfFree,
   [FirstFree |AllocatedFreqs]}}.

handle_deallocate(Freq, {FreeFreqs,
                          AllocatedFreqs}) ->

  {ok, { [Freq|FreeFreqs],
         lists:delete(Freq, AllocatedFreqs)}}.
