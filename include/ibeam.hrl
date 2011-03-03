
-record(config, { dir,
                  opts }).

-record(global_state, { working_dir }).

-define(FAIL, throw({error, failed})).

-define(ABORT(Str, Args), ibeam_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), ibeam_log:log(debug, Str, Args)).
-define(INFO(Str, Args), ibeam_log:log(info, Str, Args)).
-define(WARN(Str, Args), ibeam_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), ibeam_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).



%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).
-define(CHILD(I, Type, Arg), {I, {I, start_link, [Arg]}, permanent, brutal_kill, Type, [I]}).
