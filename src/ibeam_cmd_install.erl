%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%-------------------------------------------------------------------


-module(ibeam_cmd_install).

-behaviour(ibeam_command).

-include("ibeam.hrl").

-export([command_help/0,
         deps/0,
         run/0,
         checkpoint/0
        ]).


command_help() ->
    {"install","name=AppName vsn=AppVsn","Installs erlang release tarball found in $ROOT/AppName-AppVsn.tar.gz"}.

deps() ->
    [
     ibeam_cmd_get,
     ibeam_cmd_stage,
     ibeam_cmd_verify
    ].

checkpoint() ->
    ibeam_checkpoint:store(?MODULE).


run() ->
    ?INFO("install~n",[]),

    App = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),
    TmpDir = ibeam_config:get_global(tmp_dir),
    AppList = ibeam_config:get_global(app_info),
    SysList = ibeam_config:get_global(sys_info),
    Prefix = ibeam_config:get_global(install_prefix),
    ToInstall = ibeam_config:get_global(install_list),
    DestRoot = filename:join([Prefix,App]),
    DestLib = filename:join([DestRoot,"lib"]),
    HooksOnly = lists:member("install",string:tokens(ibeam_config:get_global(hooks_only,""),",")),

    HookArgs = ibeam_utils:make_hook_args(DestRoot, TmpDir, App, Vsn),

    ibeam_utils:hook(TmpDir,install_pre,HookArgs),

    %% the / is important. otherwise ensure_dir thinks the lib part of
    %% the path is a file
    filelib:ensure_dir(DestLib ++ "/"),

    %% make this infor permanent
    ibeam_config:set_global(dest_root, DestRoot),
    ibeam_config:set_global(dest_lib, DestLib),


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
                    ?ABORT("!!!! NOT INSTALLING ANY APPS !!!!~n",[]);
                _ ->
                    lists:foreach(fun({A,V}) ->
                                          ?DEBUG("install:~p-~s~n",[A,V])
                                  end, ToInstall),

                    ok = copy_apps(DestRoot,DestLib,ToInstall),
                    ok = copy_releases(DestRoot,{App,Vsn}),
                    ok = copy_misc(DestRoot,["lib","releases"]),


                    ibeam_utils:hook(TmpDir,install_post,HookArgs)
            end;
        true ->
            ibeam_utils:hook(TmpDir,install_post, HookArgs)
    end,
    ok.




copy_apps(RootPath,LibPath,Install) ->
    TmpDir = ibeam_config:get_global(tmp_dir),
    TmpLib = filename:join([TmpDir,"lib"]),

    lists:foreach(fun({Name,Vsn}) ->
                          %% the Vsn can be [],
                          AppName = case Vsn of
                                        [] -> atom_to_list(Name);
                                        _ -> atom_to_list(Name)++"-"++Vsn
                                    end,
                          Src = filename:join([TmpLib,AppName]),
                          Dst = LibPath,
                          ibeam_file_utils:cp_r(Src,Dst),
                          extract_manifest(RootPath,LibPath,Name,Vsn)
                  end,Install),

    ok.

copy_releases(DestRoot,{_Name,Vsn}) ->
    TmpDir = ibeam_config:get_global(tmp_dir),
    TmpRel = filename:join([TmpDir,"releases",Vsn]),
    DstRel = filename:join([DestRoot,"releases",Vsn]),
    filelib:ensure_dir(DstRel),

    ibeam_file_utils:cp_r(TmpRel,DstRel),


    %% if there is a start_erl.data then we should make sure that is in
    StartErl = filename:join([TmpDir,"releases","start_erl.data"]),
    StartDest = filename:join([DestRoot,"releases"]),
    case filelib:is_regular(StartErl) of
        true -> ibeam_file_utils:cp_r(StartErl,StartDest);
        false  -> ok
    end.





copy_misc(DestRoot,Done) ->
    TmpDir = ibeam_config:get_global(tmp_dir),
    {ok, DirList} = file:list_dir(TmpDir),
    ToCp = lists:filter(fun(L) ->
                                case lists:member(L,Done) of
                                    true -> false;
                                    false -> true
                                end
                        end,DirList),
    lists:foreach(fun(D) ->
                          Src = filename:join([TmpDir,D]),
                          Dest = filename:join([DestRoot,D]),
                          case filelib:is_dir(Src) of
                              true -> filelib:ensure_dir(Dest);
                              false -> ok
                          end,

                          ibeam_file_utils:cp_r(Src,Dest)
                  end,ToCp).



extract_manifest(RootPath,LibPath, Name,Vsn) ->
    Manifest = filename:join([LibPath,atom_to_list(Name)++"-"++Vsn,"priv","manifest"]),
    case filelib:is_regular(Manifest) of
        false ->
            ok;
        true ->
            case file:consult(Manifest) of
                {ok, [Terms]} ->
                    lists:foreach(fun(T) ->
                                          extract_manifest_tarball(RootPath,T)
                                  end,Terms),
                    ok;
                {error, E} ->
                    ?WARN("Error reading manifest file for ~p-~s: ~p~n",[Name,Vsn,E]),
                    ok
            end
    end.

extract_manifest_tarball(RootPath,{tar,Tar,Path}) ->
    Cwd = filename:join([RootPath,Path]),
    TarPath = filename:join([RootPath,Tar]),
    erl_tar:extract(TarPath,[{cwd,Cwd},compressed]).
