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
    {"verify","name=AppName vsn=AppVsn type=full|sys|none",
     "Verify the release tarball before install. ~n  full: verify that no package will overwrite or supercede any existing package installed in $ROOT. This means all apps not listed in AppNam-AppVsn/releases/AppVsn/hooks/verify as nocheck will must be the same vsn.~n  sys:  verify that all apps listed in release dir/start_clean.rel must match.~n  none: means  do not verify at all."}.

deps() -> [ibeam_cmd_get].

run() ->
    {ok, TmpDir} = extract_rel(),
    App = get_app_info(TmpDir),
    Sys = get_sys_info(App),

    Name = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),

    ibeam_utils:hook(TmpDir,{Name,Vsn},verify_pre,[App,Sys]),

    ibeam_config:set_global(app_info,App),
    ibeam_config:set_global(sys_info,Sys),
    

    VerifyType = case ibeam_config:get_global(type) of
		     undefined ->
			 ?ABORT("Verify type not specified, see help.~n",[]);
		     Type ->
			 list_to_atom(Type)
		 end,

    case verify_rel(VerifyType,App,Sys) of
	ok ->
	    ?CONSOLE("Release validated~n",[]),
	    ibeam_utils:hook(TmpDir,{Name,Vsn},verify_post,[App,Sys]),
	    ok;
	error -> error
    end.
    

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
    RelFile = ibeam_config:get_global(release_file),    
    TmpDir = ibeam_utils:mktmp_uniq(),
    case erl_tar:extract(RelFile,[{cwd,TmpDir},compressed]) of
	{error, R} -> ?ABORT("~p~n",[R]);
	ok -> ok
    end,
    ibeam_config:set_global(tmp_dir,TmpDir),
    {ok, TmpDir}.


get_sys_info([{sys,AppSys},{dep,AppDep},{app,AppApp},{erts,ErtsVsn}]) ->
    ToCheck = AppSys++AppDep++AppApp,
    AtomToVer = fun({A,_V}) ->
			case code:lib_dir(A) of
			    {error, bad_name} -> error;
			    Path ->
				DirPart = lists:last(filename:split(Path)),
				[_Name,Vsn] = string:tokens(DirPart,"-"),
				{A,Vsn}
			end
		end,
    SysSys = lists:map(AtomToVer,ToCheck),
    Rv = lists:filter(fun(AV) -> case AV of error -> false; _ -> true end end,SysSys),
    [{erts,erlang:system_info(version)}|Rv].


get_app_info(TmpDir) ->

    App = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),

    %% get the vsn of erts, kernel, stdlib from start_clean
    AppRel = filename:join([TmpDir,"releases",App++".rel"]),
    DepsRel = filename:join([TmpDir,"lib",App++"-"++Vsn,"priv","deps.rel"]),
    {ok, [{release,RelAppVsn,{erts,RelErts}, RelApps}]} = file:consult(AppRel),
    {ok, [SysDep,AppDep,AppApp]} = file:consult(DepsRel),
    

    %% verify that the app and vsn are same
    AppVsnOk = case RelAppVsn of
		   {App,Vsn} -> ok;
		   _ ->
		       ?ABORT("App and vsn in rel file do not match!~n ~p != ~p~n",[{App,Vsn},RelAppVsn])
	       end,

    [SysDep,
     AppDep,
     AppApp,
     {erts,[{erts,RelErts}]}].


verify([], App, Sys) -> ok;
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
	    

    
    

    
