-module(frequency).

-export([
         start/1,
         allocate/1,
         dealocate/2
        ]).


start(ListOfFreqs) ->
  ListOfFreqs.
  

allocate(_Freqs = [First|Rest] ) ->
  {ok, First, Rest}.

dealocate(Freq , Freqs) ->
  {ok, [Freq|Freqs]}.


