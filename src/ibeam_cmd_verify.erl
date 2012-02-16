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
         run/0
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
deps() -> [ibeam_cmd_get].

run() ->
    {ok, TmpDir} = extract_rel(),
    Name = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),
    Prefix = ibeam_config:get_global(install_prefix),
    DestDir = filename:join([Prefix,Name]),
    HooksOnly = lists:member("verify", string:tokens(ibeam_config:get_global(hooks_only,""),",")),
    HookArgs = ibeam_utils:make_hook_args(DestDir, TmpDir, Name, Vsn),
    ?INFO("verifying ~s-~s in ~s~n",[Name,Vsn,TmpDir]),


    App = get_app_info(TmpDir),
    Sys = get_sys_info(DestDir,App),

    ibeam_config:set_global(app_info,App),
    ibeam_config:set_global(sys_info,Sys),
    ibeam_config:set_global(tmp_dir,TmpDir),
    ibeam_config:set_global(dest_dir, DestDir),

    ibeam_utils:hook(TmpDir,verify_pre, HookArgs),


    case HooksOnly of
        false ->
            VerifyErts = case ibeam_config:get_global(erts) of
                             undefined ->
                                 ?ABORT("Erts verification style not specified, see help.~n",[]);
                             Erts -> list_to_atom(Erts)
                         end,
            Paths = [{global, code:root_dir()},
                     {release, TmpDir},
                     {embed, DestDir}
                    ],

            verify_erts(VerifyErts, Paths),

            VerifyType = case ibeam_config:get_global(type) of
                             undefined ->
                                 ?ABORT("Verify type not specified, see help.~n",[]);
                             Type ->
                                 list_to_atom(Type)
                         end,

            case verify_rel(VerifyType,App,Sys) of
                ok ->
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

extract_rel() ->
    RelFile = case ibeam_config:get_global(release_file) of
                  undefined ->
                      find_release();
                  Rel -> Rel
              end,
    TmpDir = ibeam_utils:mktmp_uniq(),
    case erl_tar:extract(RelFile,[{cwd,TmpDir},compressed]) of
        {error, R} -> ?ABORT("~p~n",[R]);
        ok -> ok
    end,
    ibeam_config:set_global(tmp_dir,TmpDir),
    {ok, TmpDir}.


get_sys_info(DestPath,[{sys,AppSys},{dep,AppDep},{app,AppApp},{erts,_ErtsVsn}]) ->
    ToCheck = AppSys++AppDep++AppApp,
    AtomToVer = fun({A,_V}) ->
                        case app_vsn_info(DestPath,A) of
                            undefined -> error;
                            Vsn -> {A,Vsn}
                        end
                end,
    SysSys = lists:map(AtomToVer,ToCheck),
    Rv = lists:filter(fun(AV) -> case AV of error -> false; _ -> true end end,SysSys),
    [{erts,erlang:system_info(version)}|Rv].


get_app_info(TmpDir) ->

    App = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),

    %% get the vsn of erts, kernel, stdlib from start_clean
    AppRel = filename:join([TmpDir,"releases",Vsn,App++".rel"]),
    DepsRel = filename:join([TmpDir,"lib",App++"-"++Vsn,"priv","deps.rel"]),
    {ok, [{release,RelAppVsn,{erts,RelErts}, _RelApps}]} = file:consult(AppRel),
    {ok, [SysDep,AppDep,AppApp]} = file:consult(DepsRel),


    %% verify that the app and vsn are same
    case RelAppVsn of
        {App,Vsn} -> ok;
        _ ->
            ?ABORT("App and vsn in rel file do not match!~n ~p != ~p~n",[{App,Vsn},RelAppVsn])
    end,

    [SysDep,
     AppDep,
     AppApp,
     {erts,[{erts,RelErts}]}].


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


app_vsn_info(Path, App)  when is_atom(App) ->
    app_vsn_info(Path,atom_to_list(App));
app_vsn_info(Path, App)  ->
    case filelib:wildcard(filename:join([Path,"lib",App++"-*"])) of
        [] -> undefined;
        AppVsn ->
            lists:last(string:tokens(hd(AppVsn),"-"))
    end.




find_release() ->
    Paths = case ibeam_config:get_global(rel_archive) of
        undefined ->
            Fl = ibeam_file_utils:make_default_filename(),
            [filename:join(Base, Fl) || Base <- ibeam_file_utils:default_bases()];
        FullPath ->
            [FullPath]
    end,

    %% find the first file that exists
    case lists:filter(fun filelib:is_regular/1, Paths) of
        [F|_] ->
            F;
        [] ->
            undefined
    end.


