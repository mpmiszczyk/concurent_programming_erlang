-module(frequency).

-export([
         start/1,
         allocate/1,
         deallocate/2,
         deallocate/3
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
      {ok, Freq, Freqs};
    {error, already_alocated_by, Client, Frequencies, Ref} ->
      {error, already_alocated_by, Client, Frequencies}
  after 2000 ->
      timeout
  end.

deallocate(Freq, Freqs) ->
  deallocate(Freq, Freqs, self()).

deallocate(Freq, Freqs, Client) ->
  Freqs ! {deallocate, Freq, Client}.



init(ListOfFreqs) ->
  ?MODULE:loop({{ListOfFreqs, []}, self()}).

loop({Frequencies, Server}) ->
  receive
    {allocate, Client, Ref} ->
      case handle_allocate(Frequencies, Client) of
        {ok, Allocated, NewFrequencies} ->
          Client ! {ok, Allocated, Server, Ref},
          ?MODULE:loop({NewFrequencies, Server});
        {error, already_alocated_by, Client, Frequencie} ->
          Client ! {error, already_alocated_by, Client, Frequencie, Ref},
         ?MODULE:loop({Frequencies, Server})
      end;

    {deallocate, Frequency, Client} ->
      {ok, NewFreqs} = handle_deallocate(Frequency, Client,  Frequencies),
      ?MODULE:loop({NewFreqs, Server})
  end.

handle_allocate({[FirstFree|RestOfFree],
                 AllocatedFreqs},
                Client) ->
  case alocated_before(Client, AllocatedFreqs) of
    false ->
      {ok,
       FirstFree,
       { RestOfFree,
         [{Client, FirstFree} | AllocatedFreqs]}};

    {Client, BeforeAlocated} ->
      {error,
       already_alocated_by,
       Client,
       BeforeAlocated}
  end.


alocated_before(Client, AllocatedFreqs) ->
  lists:keyfind(Client, 1, AllocatedFreqs).

handle_deallocate(Freq, Client, {FreeFreqs,
                                 AllocatedFreqs}) ->
  {ok, { [Freq|FreeFreqs],
         lists:delete({Client, Freq}, AllocatedFreqs)}}.
