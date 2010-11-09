-module(sequencer_tests).

-include_lib("eunit/include/eunit.hrl").

-import(helpers, [setup/2, setup/3]).
-import(sequencer).


% -----------------------------------------------------------------------------
sequence_path_test_() ->
    {foreach,
     fun() -> ?cmd("mktemp -d") -- "\n"  end,
     fun(T) -> ?cmd("rm -rf " ++ T) end,
     [
        fun test_sequence_path_with_no_audio_files/1,
        fun test_sequence_path_with_all_sequenced/1,
        fun test_sequence_path_with_seqnum_overflow/1,
        fun test_sequence_path_with_seqnum_overflow_best_effort/1,
        fun test_sequence_path_with_read_only_dir/1,
        fun test_sequence_path_with_non_directory_path/1,
        fun test_sequence_path_with_some_sequenced/1,
        fun test_sequence_path_with_none_sequenced/1,
        fun test_sequence_path_with_dryrun_seqnum_overflow/1,
        fun test_sequence_path_with_dryrun_seqnum_overflow_best_effort/1
     ]
    }.

test_sequence_path_with_no_audio_files(T) ->
    {"",
     fun() ->
        setup(T, [], ["a.txt", "b.pdf"]),
        ?assertEqual(
            {T, ["    * Nothing to do."], []},
            sequencer:sequence_path(T, false, 5))
     end}.

test_sequence_path_with_all_sequenced(T) ->
    {"",
     fun() ->
        setup(T, [], ["0034-c.flac @ c.flac", "099-d.mpa @ d.mpa"]),
        ?assertEqual(
            {T, ["    * Nothing to do."], []},
            sequencer:sequence_path(T, false, 5))
     end}.

test_sequence_path_with_seqnum_overflow(T) ->
    {"",
     fun() ->
        setup(T, [], ["009-a.mp3 @ a.mp3", "b.mp3"]),
        E = "    ! b.mp3 (esnfmt)",
        ?assertEqual({T, [], [E]}, sequencer:sequence_path(T, false, 1))
     end}.

test_sequence_path_with_seqnum_overflow_best_effort(T) ->
    {"",
     fun() ->
        setup(T, [], ["098-b.mp3 @ b.mp3", "a.mp3", "c.mp3"]),
        M = "    * a.mp3 -> 99-a.mp3",
        E = "    ! c.mp3 (esnfmt)",
        ?assertEqual({T, [M], [E]}, sequencer:sequence_path(T, false, 2))
     end}.

test_sequence_path_with_read_only_dir(T) ->
    {"",
     fun() ->
        setup(T, [], []),
        file:change_mode(T, 8#00400),
        E = T ++ " is not a directory or wrong permissions",
        ?assertEqual({T, [], [E]}, sequencer:sequence_path(T, false, 5))
     end}.

test_sequence_path_with_non_directory_path(T) ->
    {"",
     fun() ->
        setup(T, [], ["a"]),
        P = filename:join(T, "a"),
        E = P ++ " is not a directory or wrong permissions",
        ?assertEqual({P, [], [E]}, sequencer:sequence_path(P, false, 5))
     end}.

test_sequence_path_with_some_sequenced(T) ->
    {"",
     fun() ->
        setup(T, [],
              ["a.mp3", "b.ogg", "0034-c.flac @ c.flac", "098-d.mpa @ d.mpa"]),
        Ms = ["    * a.mp3 -> 0099-a.mp3", "    * b.ogg -> 0100-b.ogg"],
        ?assertEqual({T, Ms, []}, sequencer:sequence_path(T, false, 4))
     end}.

test_sequence_path_with_none_sequenced(T) ->
    {"",
     fun() ->
        setup(T, [], ["a.mp3", "b.ogg", "c.flac"]),
        Ms = ["    * a.mp3 -> 01-a.mp3", "    * b.ogg -> 02-b.ogg",
              "    * c.flac -> 03-c.flac"],
        ?assertEqual({T, Ms, []}, sequencer:sequence_path(T, false, 2))
     end}.

test_sequence_path_with_dryrun_seqnum_overflow(T) ->
    {"",
     fun() ->
        setup(T, [], ["009-b.mp3 @ b.mp3", "a.mp3"]),
        E = "    ! a.mp3 (esnfmt)",
        ?assertEqual({T, [], [E]}, sequencer:sequence_path(T, true, 1))
     end}.

test_sequence_path_with_dryrun_seqnum_overflow_best_effort(T) ->
    {"",
     fun() ->
        setup(T, [], ["a.mp3", "b.mp3", "098-c.mp3 @ c.mp3"]),
        M = "    . a.mp3 -> 99-a.mp3",
        E = "    ! b.mp3 (esnfmt)",
        ?assertEqual({T, [M], [E]}, sequencer:sequence_path(T, true, 2))
     end}.
