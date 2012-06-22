%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(ibeam_cmd_verify).

-behaviour(ibeam_command).

-include("ibeam.hrl").

-export([command_help/0,
         deps/0,
         run/0,
         checkpoint/0
        ]).

command_help() ->
    {"verify","name=AppName vsn=AppVsn type=full|sys|none erts=all|global|embed|none",

     "  Verify the release tarball before install. ~n~n"

     "    type determines how thoroughly we validate the existing erlang environment.~n~n"

     "      full: verify that no package will overwrite or supercede any existing package installed in $ROOT. This means all apps not listed in AppNam-AppVsn/releases/AppVsn/hooks/verify as nocheck will must be the same vsn.~n"

     "      sys:  verify that all apps listed in release dir/start_clean.rel must match.~n"

     "      none: means  do not verify at all. ~n~n"

     "    ets determines whether the global erlang install matches the embedded install. There are three erlang environments to check. The global system environment, the current embedded erlang if there is a previous install and the one in the current release. This is complicated, see README for gory details~n~n"

     "      all: check all three systems; global, installed, tarball~n"

     "      global: check that tarball is the same as global~n"

     "      embed: check that the tarball is the same as the embedded ~n"

     "      none: do not check anything.~n"
    }.
deps() ->
    [ibeam_cmd_get,
     ibeam_cmd_stage
    ].

checkpoint() ->
    ibeam_checkpoint:store(?MODULE).

run() ->
    ?INFO("verify:~n",[]),

    Name = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),
    TmpDir = ibeam_config:get_global(tmp_dir), %%tmp_dir is set by stage command
    AppList = ibeam_config:get_global(app_info),
    SysList = ibeam_config:get_global(sys_info),
    DestDir = ibeam_config:get_global(dest_dir),

    HooksOnly = lists:member("verify", string:tokens(ibeam_config:get_global(hooks_only,""),",")),
    HookArgs = ibeam_utils:make_hook_args(DestDir, TmpDir, Name, Vsn),
    ?INFO("verifying ~s-~s in ~s~n",[Name,Vsn,TmpDir]),

    ibeam_utils:hook(TmpDir,verify_pre, HookArgs),

    case HooksOnly of
        false ->
            VerifyErts = case ibeam_config:get_global(erts) of
                             undefined -> none;
                             Erts -> list_to_atom(Erts)
                         end,
            Paths = [{global, code:root_dir()},
                     {release, TmpDir},
                     {embed, DestDir}
                    ],

            verify_erts(VerifyErts, Paths),

            VerifyType = case ibeam_config:get_global(type) of
                             undefined -> none;
                             Type -> list_to_atom(Type)
                         end,

            case verify_rel(VerifyType,AppList,SysList) of
                ok ->

                    sys_config(TmpDir, Vsn),

                    ibeam_utils:hook(TmpDir,verify_post,HookArgs),
                    ok;
                error -> error
            end;
        true ->
            ibeam_utils:hook(TmpDir,verify_post,HookArgs)
    end.

verify_erts(all, Paths) ->
    verify_erts([global,embed],Paths,[]);
verify_erts(global,Paths) ->
    verify_erts([global],Paths,[]);
verify_erts(embed, Paths) ->
    verify_erts([embed],Paths,[]);
verify_erts(none, _Paths) ->
    ok.



verify_rel(full,App,Sys) ->
    verify([erts,sys,dep],App,Sys);
verify_rel(sys,App,Sys) ->
    verify([erts,sys],App,Sys);
verify_rel(none,App,Sys) ->
    verify([erts],App,Sys);
verify_rel(Type,_App,_Sys) ->
    ?ABORT("undefined verify type specified, ~p is not valid.~n",[Type]),
    error.





verify([], _App, _Sys) -> ok;
verify([Type|Types], App, Sys) ->
    case proplists:get_value(Type,App) of
        undefined ->
            {error, "Release does not have ~p in meta info.~n",[Type]};
        List ->
            case check_appvsn(List,Sys) of
                {error, {Name,Vsn,SysVsn}} ->
                    ?ABORT("~p version of ~p in release does not match system verion of ~p~n",[Name,Vsn,SysVsn]),
                    error;
                ok ->
                    verify(Types,App,Sys)
            end
    end.

check_appvsn([],_Sys) -> ok;
check_appvsn([{Name,Vsn}|List],Sys) ->
    case proplists:get_value(Name,Sys) of
        undefined ->
            %% this is ok. it means the app isn't in the system, so it should be installed.
            check_appvsn(List,Sys);
        Vsn->
            %% same vsn! all is good
            check_appvsn(List,Sys);
        SysVsn ->
            %% crap, they don't match, return error.
            {error, {Name, Vsn, SysVsn}}
    end.

verify_erts([], _Paths, _Acc) ->
    ok;
verify_erts([T|Types],  Paths, Acc) ->
    {release, [{path,RelPath},{vsn,RelVsn}]} = erts_vsn(release,Paths),
    case erts_vsn(T,Paths) of
        {T,[{path,_},{vsn,RelVsn}]} = Tf->
            %% T erts-VSN == RelVsn so all is good... continue
            verify_erts(Types,Paths,[Tf|Acc]);
        undefined ->
            %% this can be two reasons.
            %% 1. there is no global erlang... that is fine
            %% 2. there is no embed install yet... that is fine.
            verify_erts(Types,Paths,[{T,undefined}|Acc]);
        {T, [{path,RelPath},{vsn,_}]} ->
            %% wtf? how did this happen?
            ?ABORT("the erl you are using is in the temporary release, how the hell did you do that?~n",[]);
        {T,[{path,_},{vsn,TVsn}]} ->
            %% they are different erts! die
            ?ABORT("~p erts-~s is different than release erts-~s~n",[T,TVsn,RelVsn])
    end.

erts_vsn(global,_Paths) ->
    {global,[{path,code:lib_dir()},{vsn,erlang:system_info(version)}]};
erts_vsn(Type,Paths) ->
    Path = case proplists:get_value(Type,Paths) of
               undefined -> ?ABORT("~p path does not exist!?~n",[Type]);
               P -> filename:join([P,"lib"])
           end,
    case filelib:wildcard("erts-*",Path) of
        [] -> undefined;
        Erts ->
            Vsn = lists:last(string:tokens(hd(Erts),"-")),
            {Type, [{path, Path},{vsn,Vsn}]}
    end.


sys_config(TmpDir, Vsn) ->
    SysConfig = filename:join([TmpDir, "releases", Vsn, "sys.config"]),

    case file:consult(SysConfig) of
        {ok, _} ->
            ?INFO("~s is syntactically correct~n",[SysConfig]),
            ok;
        {error, Error} when is_atom(Error) ->
            ?ABORT("error in generated ~s : ~p~n",[SysConfig,Error]);
        {error, {Line, _Mod, Term}} ->
            Msg = case Term of
                      Term when is_list(Term) -> lists:flatten(Term);
                      _ -> Term
                  end,
            ?ABORT("~s:~p: ~p~n",[SysConfig, Line, Msg])
    end.
