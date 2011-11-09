
-module(ibeam).

-export([
         main/1
        ]).

main(Args) ->
    %% we always need to load the app, so just load it here
    ok = application:load(ibeam),
    case catch(ibeam_core:run(Args)) of
        ok ->
            ok;
        {error, failed} ->
            halt(1);
        Error ->
            io:format("Uncaught error in ibeam_core: ~p~n",[Error]),
            halt(1)
    end.
