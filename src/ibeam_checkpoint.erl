%%%-------------------------------------------------------------------
%%% @author  Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2012,
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2012 by  Justin Kirby <jkirby@voalte.com>
%%%-------------------------------------------------------------------
-module(ibeam_checkpoint).


-export([
         load/1,
         store/1,
         maybe_run/1
         ]).

-include("ibeam.hrl").

load(NoOverride) ->

    Fname = checkpoint_name(),

    case file:consult(Fname) of
        {ok, Env} ->
            ?INFO("LOADED: ~s~n",[Fname]),
            [ibeam_config:set_global(K,V) || {K,V} <- Env, not(lists:member(K,NoOverride))];
        {error,_} ->
            ?INFO("NO checkpoint file exists~n",[])
    end.

store(Cmd) ->
    Fname = checkpoint_name(),

    ibeam_config:set_global(checkpoint_commands, lists:usort([Cmd|ibeam_config:get_global(checkpoint_commands, [])])),

    ToStore = [io_lib:format("~p.~n",[E]) || E <- application:get_all_env(ibeam)],

    ok = file:write_file(Fname, ToStore).


maybe_run(Cmd) ->
    case ibeam_config:get_global(force, false) of
        false ->
            not(lists:member(Cmd, ibeam_config:get_global(checkpoint_commands,[])));
        true -> true
    end.




checkpoint_name() ->
    {App, Vsn} = ibeam_utils:app_vsn_throw(),
    CheckPath = ibeam_config:get_global(checkpoint),

    lists:flatten(io_lib:format(CheckPath, [App, Vsn])).
