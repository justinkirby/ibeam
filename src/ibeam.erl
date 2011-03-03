
-module(ibeam).

-export([
	 main/1
	 ]).

main(Args) ->
    case catch(ibeam_core:run(Args)) of
	ok ->
	    ok;
	{error, failed} ->
	    halt(1);
	Error ->
	    io:format("Uncaught error in ibeam_core: ~p~n",[Error]),
	    halt(1)
    end.
