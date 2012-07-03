%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%%-------------------------------------------------------------------


-module(ibeam_cmd_clean).

-behaviour(ibeam_command).

-include("ibeam.hrl").

-export([command_help/0,
         deps/0,
         run/0,
         checkpoint/0
        ]).


command_help() ->
    Help = "Clean will remove the files and directories generated as a side effect of staging and installation. This means the following will be deleted: ~n  - stage directory~n  - release tarball~n  - checkpiont file~n",
    {"clean","name=AppName vsn=AppVsn",Help}.

deps() -> [].

checkpoint() ->
    ibeam_checkpoint:store(?MODULE).


run() ->

    {App, Vsn} = ibeam_utils:app_vsn_throw(),
    TmpDir = ibeam_config:get_global(tmp_dir),
    DestRoot = ibeam_config:get_global(dest_root),
    HookArgs = ibeam_utils:make_hook_args(DestRoot, TmpDir, App, Vsn),
    HooksOnly = lists:member("clean",string:tokens(ibeam_config:get_global(hooks_only,""),",")),


    ibeam_utils:hook(TmpDir,clean_pre,HookArgs),

    case HooksOnly of
        false -> run2();
        _ -> null
    end,

    ibeam_utils:hook(TmpDir,clean_post,HookArgs).

run2() ->

    %% do we have anything to clean?  determine that by whetehre there
    %% is a checkpoint file for the app-vsn

    case ibeam_checkpoint:exist() of
        false ->
            %% there is no checkpoint file. Therefore, there is nothing to try to delete.
            ok;
        true ->
            %% ok, we have a checkpoint file so lets delete things in reverse order.
            rm_tmp_dir(),
            rm_rel_ball(),
            rm_checkpoint()
    end.

rm_tmp_dir() ->
    TmpDir = ibeam_config:get_global(tmp_dir),
    ibeam_file_utils:rm_rf(TmpDir).

rm_rel_ball() ->
    RelFile = ibeam_config:get_global(release_file),
    ibeam_file_utils:rm_rf(RelFile).

rm_checkpoint() ->
    CpFile = ibeam_checkpoint:name(),
    ibeam_file_utils:rm_rf(CpFile).
