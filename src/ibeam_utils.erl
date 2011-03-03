-module(ibeam_utils).

-include("ibeam.hrl").

-export([
	 hook/4,
	 app_in_sys/1,
	 mktmp_uniq/0,
	 sh/2,
	 find_executable/1,
	 log_and_abort/2,
	 abort/2
	 ]).

hook(Dir, {Name,Vsn}, Hook, Args) ->
    
    HookDir = filename:join([Dir,"lib",Name++"-"++Vsn,"priv","ibeam_hooks"]),
    HookName = atom_to_list(Hook),
    HookFile = filename:join([HookDir,HookName]),
    case filelib:is_regular(HookFile++".erl") of
	false ->
	    ?CONSOLE("~p does not exist~n",[HookFile++".erl"]),
	    ok;
	true ->
	    {ok,Old} = file:get_cwd(),
	    file:set_cwd(HookDir),
	    {ok,Hook} = compile:file(HookFile++".erl"),
	    {module,Mod} = code:load_abs(HookFile),
	    file:set_cwd(Old),
	    Mod:hook(Args)
    end.
		
		
    

app_in_sys({Name,Vsn}) ->
    Lib = code:lib_dir(),
    App = filename:join(Lib,Name++"-"++Vsn),

    filelib:is_dir(App).

mktmp_uniq() ->
    TempBase = case os:getenv("TEMP") of
		   false ->
		       "/tmp";
		   Tb -> Tb
	       end,
    Uniq = integer_to_list(erlang:phash2({node(),now()})),
    UniqPath = filename:join([TempBase,"ibeam_"++Uniq]),
    ok = filelib:ensure_dir(UniqPath),
    UniqPath.

sh(Command0, Options0) ->
    ?INFO("sh: ~s\n~p\n", [Command0, Options0]),

    DefaultOptions = [use_stdout, abort_on_error],
    Options = [expand_sh_flag(V)
               || V <- proplists:compact(Options0 ++ DefaultOptions)],

    ErrorHandler = proplists:get_value(error_handler, Options),
    OutputHandler = proplists:get_value(output_handler, Options),

    Command = patch_on_windows(Command0),
    PortSettings = proplists:get_all_values(port_settings, Options) ++
        [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = open_port({spawn, Command}, PortSettings),

    case sh_loop(Port, OutputHandler, []) of
        {ok, _Output} = Ok ->
            Ok;
        {error, Rc} ->
            ErrorHandler(Command, Rc)
    end.

find_executable(Name) ->
    case os:find_executable(Name) of
        false -> false;
        Path ->
            "\"" ++ filename:nativename(Path) ++ "\""
    end.


-spec abort(string(), [term()]) -> no_return().
abort(String, Args) ->
    ?ERROR(String, Args),
    halt(1).

-spec log_and_abort(string(), integer()) -> no_return().
log_and_abort(Command, Rc) ->
    ?ABORT("~s failed with error: ~w\n", [Command, Rc]).



expand_sh_flag(return_on_error) ->
    {error_handler,
     fun(_Command, Rc) ->
             {error, Rc}
     end};
expand_sh_flag({abort_on_error, Message}) ->
    {error_handler,
     fun(_Command, _Rc) ->
             ?ABORT(Message, [])
     end};
expand_sh_flag(abort_on_error) ->
    {error_handler,
     fun log_and_abort/2};
expand_sh_flag(use_stdout) ->
    {output_handler,
     fun(Line, Acc) ->
             ?CONSOLE("~s", [Line]),
             [Acc | Line]
     end};
expand_sh_flag({use_stdout, false}) ->
    {output_handler,
     fun(Line, Acc) ->
             [Acc | Line]
     end};
expand_sh_flag({cd, _CdArg} = Cd) ->
    {port_settings, Cd};
expand_sh_flag({env, _EnvArg} = Env) ->
    {port_settings, Env}.


patch_on_windows(Cmd) ->
    case os:type() of
        {win32,nt} ->
            case find_executable("bash") of
                false -> Cmd;
                Bash ->
                    Bash ++ " -c \"" ++ Cmd ++ "; echo _port_cmd_status_ $?\" "
            end;
        _ ->
            Cmd
    end.



sh_loop(Port, Fun, Acc) ->
    receive
        {Port, {data, {_, "_port_cmd_status_ " ++ Status}}} ->
            (catch erlang:port_close(Port)), % sigh () for indentation
            case list_to_integer(Status) of
                0  -> {ok, lists:flatten(Acc)};
                Rc -> {error, Rc}
            end;
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line, Acc));
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(Acc)};
        {Port, {exit_status, Rc}} ->
            {error, Rc}
    end.
