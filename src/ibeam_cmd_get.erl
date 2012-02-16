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
    HelpDesc = io_lib:format("Fetches erlang release tarball found in ~s/AppName/AppName-AppVsn.tar.gz unless overridden by -A. If url parameter is specified it will just fetch that.",[Repos]),
    {"get","name=AppName vsn=AppVsn url=http://FullUrl.com/path",HelpDesc}.

deps() -> [].


run() ->
    Dest = ibeam_file_utils:make_archive_filename(),

    %% does the file exist?
    %% if so, then only download it again if force is used
    Skip = fetch_skip(Dest),
    Source = fetch_source(),

    case fetch_sh(Dest,Source,Skip) of
        {ok,Sh} ->
            ibeam_utils:sh(Sh,[]);
        skip ->
            ok
    end,

    ibeam_config:set_global(release_file,Dest),

    ok.

fetch_source() ->
    case ibeam_config:get_global(local_file) of
        true ->
            {cp, ibeam_file_utils:make_default_filename()};
        _ ->
            {wget, fetch_url()}
    end.


fetch_url() ->
    case ibeam_config:get_global(url) of
        undefined ->
            UrlTemplate = ibeam_config:get_global(repos),
            {App, Vsn} = ibeam_utils:app_vsn_throw(),
            ?FMT(UrlTemplate,[App,Vsn,App,Vsn]);
        GUrl ->
            GUrl
    end.

%% Skip fetching if
%% - File exists and
%% - We are not forcing a fetch and
%% - We've not specified that the file is local.
fetch_skip(Dest) ->
    filelib:is_regular(Dest) andalso
    (not ibeam_config:get_global(force, false)) andalso
    (not ibeam_config:get_global(local_file, false)).

fetch_sh(Dest,_Src,true) ->
    ?WARN("~s exists, skipping get~n",[Dest]),
    skip;
fetch_sh(Filename,{cp,Filename},false) ->
    ?ABORT("Source and dest filenames are the same (~s)~n",[Filename]);
fetch_sh(Dest,{cp,Src},false) ->
    {ok,?FMT("cp -fR ~s ~s",[Src,Dest])};
fetch_sh(Dest,{wget,Src},false) ->
    {ok,?FMT("wget --no-check-certificate -nv -O ~s ~s",[Dest,Src])};
fetch_sh(_,Src,false) ->
    ?ABORT("~s is an invalid source~n",[Src]).
