%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%
%%%-------------------------------------------------------------------


-module(ibeam_cmd_get).

-behaviour(ibeam_command).

-include("ibeam.hrl").

-export([command_help/0,
	 deps/0,
	 run/0
	 ]).


command_help() ->
    Repos = ibeam_config:get_global(repos),
    HelpDesc = io_lib:format("Fetches erlang release tarball found in ~s/AppName/AppName-AppVsn.tar.gz    If url parameter is specified it will just fetch that.",[Repos]),
    {"get","name=AppName vsn=AppVsn url=http://FullUrl.com/path",HelpDesc}.

deps() -> [].
    

run() ->
    ?INFO("get~n",[]),


    App = ibeam_config:get_global(name),
    Vsn = ibeam_config:get_global(vsn),
    Url = case ibeam_config:get_global(url) of
	      undefined ->
		  UrlTemplate = ibeam_config:get_global(repos),
		  ?FMT(UrlTemplate,[App,App,Vsn]);
	      GUrl -> GUrl
	  end,
    
    Root = code:root_dir(),
    RelName = App ++ "-" ++ Vsn,
    Dest = filename:join([Root,RelName++".tar.gz"]),

    %% does the file exist?
    %% if so, then only download it again if force is used
    Skip = case filelib:is_regular(Dest) of
	       true ->
		   case ibeam_config:get_global(force) of
		       undefined -> true;
		       _ -> false
		   end;
	        _ -> false	    
	   end,

    case Skip of
	true ->
	    ?CONSOLE("~s exists, skipping get~n",[Dest]),
	    ok;
	false ->
	    ibeam_utils:sh(?FMT("wget -nv -O ~s ~s",[Dest,Url]),[])
    end,
    ibeam_config:set_global(release_file,Dest),

    ok.    
