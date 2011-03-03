%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011 Justin Kirby
%%% @end
%%%-------------------------------------------------------------------


-module(ibeam_command).


-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{command_help,0},
     {deps,0},
     {run,0}     
    ];
behaviour_info(_Other) ->
    undefined.
