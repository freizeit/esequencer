#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname main

-module(main).
-author('mh@foldr3.com').
-export([main/1]).

-define(SPEC,
        [{width, $w, "width", {integer, 5},
          "width of numeric prefix, default: 5"},
         {dryrun, $n, "dry-run", undefined,
          "only show what needs done, leave files unchanged"},
         {help, $h, "help", undefined, "display this help and exit"}]).

main(Arguments) ->
    Pwd = filename:dirname(escript:script_name()),
    true = code:add_pathz(Pwd),
    case getopt:parse(?SPEC, Arguments) of
        {ok, {Opts, Args}} ->
            maybe_help(Opts),
            Paths = case Args of
                [] -> ["."];
                _ -> filters:filter_paths(Args)
                end,
            Dryrun = proplists:get_bool(dryrun, Opts),
            Width = proplists:get_value(width, Opts),
            sequencer:sequence_paths(Paths, Dryrun, Width);
        {error, _} ->
            usage(1)
   end.

maybe_help(Opts) ->
  case proplists:get_bool(help, Opts) of
      true -> usage(0);
      false -> ok
  end.

usage(Exit) ->
   getopt:usage(
       ?SPEC, "sequencer", "[paths]"),
   case Exit of
       0 ->
         io:format("Looks for audio files in the specified paths and "
                   "prepends them with a\nnumeric sequence of the given "
                   "width.\n");
       _ -> ok
   end,
   erlang:halt(Exit).
