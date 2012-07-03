
-module(ibeam_core).


-export([run/1]).

-include("ibeam.hrl").

-ifndef(BUILD_TIME).
-define(BUILD_TIME, "undefined").
-endif.

-ifndef(VCS_INFO).
-define(VCS_INFO, "undefined").
-endif.

run(["help"]) ->
    help(),
    ok;
run(["version"]) ->
    version(),
    ok;
run(Args) ->

    Commands = parse_args(Args),
    ibeam_log:init(),

    %% list of vars we do NOT want to override is what is specified on
    %% the command line. So get them from the list and pass them into
    %% load
    NoOverride = [K || {K, _, _, _, _} <- option_spec_list()],
    ibeam_checkpoint:load(NoOverride),

    CmdPre = ibeam_config:get_global(command_prefix),

    CommandAtoms = [list_to_atom(CmdPre++C) || C <- Commands],

    process_commands(CommandAtoms),
    ok.

process_commands(Commands) ->
    process_commands(Commands,[]).

process_commands([],History) -> History;

process_commands([Command|Rest],History) ->
    case lists:member(Command,History) of
        true -> process_commands(Rest,History);
        false ->
            NewHistory = case ibeam_config:get_global(noauto) of
                             true -> [];
                             _ ->
                                 Pre = Command:deps(),
                                 process_commands(Pre,History)
                         end,

            case ibeam_checkpoint:maybe_run(Command) of
                false ->
                    ?INFO("Command ~p is checkpointed, use -f to force~n",[Command]);
                true ->
                    Command:run(),
                    Command:checkpoint()
            end,
            process_commands(Rest,[Command|NewHistory])
    end.



parse_args(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList,Args) of
        {ok,{Options,NonOptArgs}} ->
            {ok,continue} = show_info_maybe_halt(Options,NonOptArgs),
            options_set(Options),
            fix_interdependencies(),
            filter_flags(NonOptArgs,[]);

        {error, {Reason,Data}} ->
            ?ERROR("Error: ~s ~p~nAn",[Reason,Data]),
            help(),
            halt(1)
    end.

show_info_maybe_halt(Opts, NonOptArgs) ->
    case proplists:get_bool(help, Opts) of
        true ->
            help(),
            halt(0);
        false ->
            case proplists:get_bool(version,Opts) of
                true ->
                    version(),
                    halt(0);
                false ->
                    case NonOptArgs of
                        [] ->
                            ?CONSOLE("No command specified!~n",[]),
                            help(),
                            halt(1);
                        _ ->
                            {ok,continue}
                    end
            end
    end.

options_set([]) ->
    ok;
options_set([Opt|Rest]) ->
    case Opt of
        {Key,Value} ->
            ibeam_config:set_global(Key,Value);
        Key ->
            ibeam_config:set_global(Key,true)
    end,
    options_set(Rest).


filter_flags([],Commands) ->
    lists:reverse(Commands);
filter_flags([Item | Rest], Commands) ->
    case string:tokens(Item, "=") of
        [Command] ->
            filter_flags(Rest, [Command | Commands]);
        [KeyStr, Value] ->
            Key = list_to_atom(KeyStr),
            ibeam_config:set_global(Key,Value),
            filter_flags(Rest,Commands);
        Other ->
            ?CONSOLE("Ignoring command line argument: ~p\n",[Other]),
            filter_flags(Rest,Commands)
    end.





version() ->
    {ok, Vsn} = application:get_key(ibeam, vsn),
    ?CONSOLE("ibeam version: ~s~n",[Vsn]).

help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "ibeam",
                 "[var=value ...] [command,...]",
                 [{"var=value","ibeam global variables (e.g. cookie=fooo)"},
                  {"command,...","Command to run (e.g. install)"}]),
    commands_usage().

commands_usage() ->
    CommandModules = lists:map(fun(C) ->
                                       M = list_to_atom("ibeam_cmd_"++atom_to_list(C)),
                                       M:command_help()
                               end, ibeam_config:get_global(commands)),
    lists:foreach(fun({Cmd, Args, Desc}) ->
                          ?CONSOLE("  ~-15.. s~s~n  ~s~n~n",[Cmd,Args,io_lib:format(Desc,[])])
                  end,CommandModules),
    ok.


option_spec_list() ->
    [
     {help, $h, "help", undefined, "Display help message."},
     {version, $V, "version", undefined, "Display version."},
     {verbose, $v, "verbose", integer, "verbose logging output, 0-3 0=error,3=debug"},
     {force, $f, "force", undefined, "Skip all safety checks and start from the beginning."},
     {rel_archive, $A, "rel-archive", string, "Override default release archive file name to install. Implies/forces --local|-l"},
     {local_file, $l, "local", undefined, "Use local filesystem instead of url."},
     {noauto, $a, "noauto", undefined, "Do not automatically run dependent commands."},
     {preserve, $P, "preserve", undefined, "Preserve temporary files and directories."},
     {hook_args, $H, "hookargs", undefined, "Extra arguments to pass to hooks."}
    ].

fix_interdependencies() ->
    case ibeam_config:get_global(rel_archive) of
        undefined ->
            ok;
        _ -> % If rel_archive is set, it implies a local file, so force it
            ibeam_config:set_global(local_file, true) % Sorry for this kluge
    end,
    ok.

cleanup() ->
    case ibeam_config:get_global(preserve) of
        true ->
            ok;
        _ ->
            ibeam_file_utils:rmtmp_uniq()
    end.

