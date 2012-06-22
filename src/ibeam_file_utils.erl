-module(ibeam_file_utils).

-include("ibeam.hrl").

-export([
        rm_rf/1,
        rmtmp_uniq/0,
        cp_r/2,
        make_archive_filename/0,
        make_default_filename/0,
        default_bases/0
	 ]).

path_sep() -> filename:join(["a","b"]) -- "ab".

default_bases() ->
    [ibeam_utils:tmp_base(), "rel"].


rm_rf(Target) ->
    case os:type() of
        {unix, _} ->
            {ok, []} = ibeam_utils:sh(?FMT("rm -rf ~s", [Target]),
                                      [{use_stdout, false}, return_on_error]),
            ok;
	OS ->
	    ?ABORT("Do not know how to rm_rf on ~p~n",[OS]),
	    ok
    end.

%% do NOT use this for renaming!
cp_r(Sources, Dest) ->
    case os:type() of
        {unix, _} ->
            {ok, []} = ibeam_utils:sh(?FMT("cp -fR ~s ~s", [Sources, munge_dest(Sources, Dest)]),
                                      [{use_stdout, false}, return_on_error]),
            ok;
	OS ->
	    ?ABORT("Do not know how to cp_r on ~p~n",[OS]),
	    ok
    end.

rmtmp_uniq() ->
    case ibeam_config:get_global(tmp_dir) of
        undefined ->
            ok;
        TmpDir ->
            rm_rf(TmpDir)
    end.

make_archive_filename() ->
    filename:join(ibeam_utils:tmp_base(), make_default_filename()).

make_default_filename() ->
    {App, Vsn} = ibeam_utils:app_vsn_throw(),
    App ++ "-" ++ Vsn ++ ".tar.gz".

%% this is complicated so bear with me.  the cp_r can make nested
%% dirs.  this means that cp -fR /tmp/foo /opt/foo MAY create
%% /opt/foo/foo IF the destinations exists and is a dir.
%%
%% The solution is to do the following:
%% - if the last path of the source and destination are the same, e.g. /tmp/foo -> /opt/foo
%%   - if the dest exists AND is a dir then remove the last path component from dest
%% /tmp/foo -> /opt/foo  becomes /tmp/foo -> /opt

%% - if the src is a dir and the last path component are not the same,
%% then append a / to the dest to make sure the path is created.

munge_dest(Source, Dest) ->
    SrcLast = lists:last(filename:split(Source)),
    DestLast = lists:last(filename:split(Dest)),

    case SrcLast == DestLast of
        false ->
            %% are not the same, then check to see if Source is a dir,
            %% if so append / to Dest
            case filelib:is_dir(Source) of
                false -> Dest;
                true ->
                    Dest ++ path_sep()
            end;
        true ->
            case filelib:is_dir(Dest) of
                false -> Dest;
                true ->
                    %% remove the last elem of the list:
                    filename:join(lists:reverse(tl(lists:reverse(filename:split(Dest)))))
            end
    end.
