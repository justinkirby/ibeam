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
    

    ibeam_utils:hook(TmpDir,{App,Vsn},install_pre,[TmpDir,App,Vsn]),

    AppList = ibeam_config:get_global(app_info),
    SysList = ibeam_config:get_global(sys_info),
    Prefix = ibeam_config:get_global(install_prefix),


    

    

%%    ToInstall = install_list(AppList,SysList),

%%    ok = copy_apps(ToInstall),
%%    ok = copy_releases({App,Vsn}),

    
    ibeam_utils:hook(code:root_dir(),{App,Vsn},install_post,[code:lib_dir(),App,Vsn]),
    ?CONSOLE("Install: ~p~n",[ToInstall]),

    ok.


install_list(App, Sys) ->
    %% note we never touch 'sys' apps.  The verify type of full|sys should have been run.
    %% if you want to make sure the environment is setup, then verify with type=full|sys
    All = proplists:get_value(dep,App,[]) ++ proplists:get_value(app,App,[]),
    case ibeam_config:get_global(force,false) of
	true ->
	    %% if force is specified, then just install everything
	    All;
	false ->
	    install_list_nice(All,Sys,[])
    end.

install_list_nice([],Sys,Install) -> Install;
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
	

    
	    
copy_apps(Install) ->
    TmpDir = ibeam_config:get_global(tmp_dir),
    TmpLib = filename:join([TmpDir,"lib"]),

    lists:foreach(fun({Name,Vsn}) ->
			  Src = filename:join([TmpLib,atom_to_list(Name)++"-"++Vsn]),
			  Dst = code:lib_dir(),
			  Res = ibeam_file_utils:cp_r(Src,Dst),
			  ?CONSOLE("CP res: ~p~n",[Res])			      
		  end,Install),

    ok.

copy_releases({Name,Vsn}) ->
    TmpDir = ibeam_config:get_global(tmp_dir),
    TmpRel = filename:join([TmpDir,"releases",Vsn,"*"]),
    DstRel = filename:join([code:root_dir(),"releases",Name++"-"++Vsn]),
    ibeam_file_utils:cp_r(TmpRel,DstRel).
