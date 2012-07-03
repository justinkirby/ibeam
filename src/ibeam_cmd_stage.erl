%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%-------------------------------------------------------------------


-module(ibeam_cmd_stage).

-behaviour(ibeam_command).

-include("ibeam.hrl").

-export([command_help/0,
         deps/0,
         run/0,
         checkpoint/0
        ]).


command_help() ->
    {"stage","name=AppName vsn=AppVsn","Stages erlang release tarball found in $ROOT/AppName-AppVsn.tar.gz in a mktmp dir. Action info is recorded in the /tmp/.ibeam.stage file"}.

deps() ->
    [
     ibeam_cmd_get
    ].

checkpoint() ->
    ibeam_checkpoint:store(?MODULE).


run() ->
    ?INFO("stage~n",[]),

    {ok, TmpDir} = extract_rel(),


    App = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),
    Prefix = ibeam_config:get_global(install_prefix),
    DestRoot = filename:join([Prefix,App]),
    ibeam_config:set_global(dest_dir, DestRoot),
    HooksOnly = lists:member("stage",string:tokens(ibeam_config:get_global(hooks_only,""),",")),

    HookArgs = ibeam_utils:make_hook_args(DestRoot, TmpDir, App, Vsn),

    ibeam_utils:hook(TmpDir,stage_pre,HookArgs),




    AppList = get_app_info(TmpDir),
    SysList = get_sys_info(DestRoot,AppList),
    ibeam_config:set_global(app_info,AppList),
    ibeam_config:set_global(sys_info,SysList),


    ToInstall = install_list(AppList,SysList),


    case HooksOnly of
        false ->
            lists:foreach(fun({A,V}) ->
                                  ?DEBUG("sys:~p-~s~n",[A,V])
                          end, SysList),
            lists:foreach(fun({A,V}) ->
                                  ?DEBUG("app:~p-~s~n",[A,V])
                          end, proplists:get_value(sys,AppList)),

            case ToInstall of
                [] ->
                    ?ABORT("!!!! NOT STAGING/INSTALLING ANY APPS !!!!~n",[]);
                _ ->


                    stage_complete(TmpDir, ToInstall, HookArgs)
            end;
        true ->
            stage_complete(TmpDir, ToInstall, HookArgs)
    end,
    ok.


stage_complete(TmpDir, Install, HookArgs) ->
    ibeam_config:set_global(install_list, Install),
    ibeam_utils:hook(TmpDir, stage_post, HookArgs).

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




install_list(App, Sys) ->
    %% note we check sys apps because there might be nothing there,
    %% e.g. first install
    All = proplists:get_value(dep,App,[]) ++
        proplists:get_value(app,App,[]) ++
        proplists:get_value(sys,App,[]),
    case ibeam_config:get_global(force,false) of
        true ->
            %% if force is specified, then just install everything
            All;
        false ->
            install_list_nice(All,Sys,[])
    end.

install_list_nice([],_Sys,Install) -> Install;
install_list_nice([{Name,Vsn}|App],Sys,Install) ->
    SysVsn =  proplists:get_value(Name,Sys),
    Add = case SysVsn of
              %% it is not there, so we install it
              undefined ->true;

              %% same vsn, so we ignore it
              Vsn -> false;

              %% Vsn in rel is > installed, upgrade it
              SysVsn when Vsn > SysVsn -> true;

              %% Vsn in rel is < installed! wtf, abort.
              SysVsn when Vsn < SysVsn ->
                  ?ABORT(" ~p-~s is older than ~p-~s. Use -f to force the install.~n",[Name,Vsn,Name,SysVsn]),
                  false
          end,
    if
        Add =:= true ->
            install_list_nice(App,Sys,[{Name,Vsn}|Install]);
        true ->
            install_list_nice(App,Sys,Install)
    end.

find_release() ->
    %% look in /tmp/App-Vsn.tar.gz then look in rel/App-Vsn.tar.gz
    %% then give up
    App = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),
    Fl = App ++ "-" ++ Vsn ++".tar.gz",
    Tmp = filename:join(["/tmp",Fl]),
    Local = filename:join(["rel",Fl]),

    %% find the first file that exists
    Found = [F || F <- [Tmp,Local], filelib:is_regular(F)],
    case Found of
        [] -> undefined;
        L -> hd(L)
    end.


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

app_vsn_info(Path, App)  when is_atom(App) ->
    app_vsn_info(Path,atom_to_list(App));
app_vsn_info(Path, App)  ->
    case filelib:wildcard(filename:join([Path,"lib",App++"-*"])) of
        [] -> undefined;
        AppVsn ->
            lists:last(string:tokens(hd(AppVsn),"-"))
    end.
