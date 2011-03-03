-module(ibeam_file_utils).

-include("ibeam.hrl").

-export([rm_rf/1,
	 cp_r/2
	 ]).



rm_rf(Target) ->
    case os:type() of
        {unix, _} ->
            {ok, []} = rebar_utils:sh(?FMT("rm -rf ~s", [Target]),
                                      [{use_stdout, false}, return_on_error]),
            ok;
	OS ->
	    ?ABORT("Do mot know how to rm_rf on ~p~n",[OS]),
	    ok
    end.


cp_r(Sources, Dest) ->
    case os:type() of
        {unix, _} ->
%%            SourceStr = string:join(Sources, " "),
            {ok, []} = ibeam_utils:sh(?FMT("cp -R ~s ~s", [Sources, Dest]),
                                      [{use_stdout, false}, return_on_error]),
            ok;
	OS ->
	    ?ABORT("Do not know how to cp_r on ~p~n",[OS]),
	    ok
    end.
