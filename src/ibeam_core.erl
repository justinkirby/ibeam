
-module(ibeam_core).

-include("ibeam.hrl").

-export([run/1]).

-ifndef(BUILD_TIME).
-define(BUILD_TIME, "undefined").
-endif.

-ifndef(VCS_INFO).
-define(VCS_INFO, "undefined").
-endif.

run(["help"]) ->
    ok = application:load(ibeam),
    help(),
    ok;
run(["version"]) ->
    ok = application:load(ibeam),
    version(),
    ok;
run(Args) ->
    ok = application:load(ibeam),


    Commands = parse_args(Args),

%%    ok = crypto:start(),
    %% start logger when we have it

    application:start(ibeam),
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
	    Pre = Command:deps(),
	    NewHistory = process_commands(Pre,History),
	    Command:run(),
	    process_commands(Rest,[Command|NewHistory])
    end.
    


parse_args(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList,Args) of
	{ok,{Options,NonOptArgs}} ->
	    {ok,continue} = show_info_maybe_halt(Options,NonOptArgs),
	    options_set(Options),
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
			  ?CONSOLE("  ~-15.. s~s~n  ~s~n~n",[Cmd,Args,Desc])
		  end,CommandModules),
    ok.


option_spec_list() ->
    [
     {help, $h, "help", undefined, "Display help message."},
     {force, $f, "force", undefined, "Skip all safety checks and start from the beginning."}
    ].
