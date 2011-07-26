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
	 run/0
	 ]).


command_help() ->
    {"install","name=AppName vsn=AppVsn","Installs erlang release tarball found in $ROOT/AppName-AppVsn.tar.gz"}.

deps() ->
    [
     ibeam_cmd_get,
     ibeam_cmd_verify
    ].


run() ->
    ?INFO("install~n",[]),

    App = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),
    TmpDir = ibeam_config:get_global(tmp_dir),
    AppList = ibeam_config:get_global(app_info),
    SysList = ibeam_config:get_global(sys_info),
    Prefix = ibeam_config:get_global(install_prefix),
    DestRoot = filename:join([Prefix,App]),
    DestLib = filename:join([DestRoot,"lib"]),

    ibeam_utils:hook(TmpDir,{App,Vsn},install_pre,[TmpDir,App,Vsn]),

    %% the / is important. otherwise ensure_dir thinks the lib part of
    %% the path is a file
    filelib:ensure_dir(DestLib ++ "/"),




    ToInstall = install_list(AppList,SysList),


    ?INFO("SYSLIST ~n~p~n",[SysList]),
    ?INFO("APPLIST ~n~p~n",[AppList]),
    ?INFO("INSTALL list~n~p~n.",[ToInstall]),

    ok = copy_apps(DestRoot,DestLib,ToInstall),
    ok = copy_releases(DestRoot,{App,Vsn}),
    ok = copy_misc(DestRoot,["lib","releases"]),



    ibeam_utils:hook(DestRoot,{App,Vsn},install_post,[DestLib,App,Vsn]),

    ok.


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




copy_apps(RootPath,LibPath,Install) ->
    TmpDir = ibeam_config:get_global(tmp_dir),
    TmpLib = filename:join([TmpDir,"lib"]),

    lists:foreach(fun({Name,Vsn}) ->
			  Src = filename:join([TmpLib,atom_to_list(Name)++"-"++Vsn]),
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





