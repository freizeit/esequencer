-module(search_tests).

-include_lib("eunit/include/eunit.hrl").

-import(helpers, [setup/2, setup/3]).
-import(search).

% -----------------------------------------------------------------------------
audio_files_test_() ->
    {foreach,
     fun() -> ?cmd("mktemp -d") -- "\n"  end,
     fun(T) -> ?cmd("rm -rf " ++ T) end,
     [
        fun test_audio_files_with_flac/1,
        fun test_audio_files_with_sequenced_flac/1,
        fun test_audio_files_with_equal_mtime_sorts_on_filename/1,
        fun test_audio_files_with_matching_dirs/1,
        fun test_audio_files_with_matching_files_and_dirs/1,
        fun test_audio_files_with_mp3/1,
        fun test_audio_files_with_mpa/1,
        fun test_audio_files_with_sequenced_mp3/1,
        fun test_audio_files_with_sequenced_mpa/1,
        fun test_audio_files_with_multiple_different_kind/1,
        fun test_audio_files_with_multiple_same_kind/1,
        fun test_audio_files_with_multiple_sorts_oldest_to_newest/1,
        fun test_audio_files_with_no_files/1,
        fun test_audio_files_with_non_directory_path/1,
        fun test_audio_files_with_non_existent_path/1,
        fun test_audio_files_with_non_matching/1,
        fun test_audio_files_with_non_writable_directory/1,
        fun test_audio_files_with_ogg/1,
        fun test_audio_files_with_sequenced_ogg/1
     ]
    }.

test_audio_files_with_no_files(T) ->
    {"In the absence of any files an empty list is returned.",
     fun() ->
        ?assertEqual({ok, []}, search:audio_files(T))
     end}.

test_audio_files_with_non_matching(T) ->
    {"Files with non-matching names are ignored.",
     fun() ->
        setup(T, [], ["a.txt", "x.pdf"]),
        ?assertEqual({ok, []}, search:audio_files(T))
     end}.

test_audio_files_with_mp3(T) ->
    {".mp3 files are returned",
     fun() ->
        setup(T, [], ["a.mp3", "x.pdf"]),
        ?assertEqual({ok, ["a.mp3"]}, search:audio_files(T))
     end}.

test_audio_files_with_sequenced_mp3(T) ->
    {".mp3 files are returned",
     fun() ->
        setup(T, [], ["00004-a.mp3 @ a.mp3", "x.pdf"]),
        ?assertEqual({ok, ["00004-a.mp3"]}, search:audio_files(T))
     end}.

test_audio_files_with_ogg(T) ->
    {".ogg files are returned",
     fun() ->
        setup(T, [], ["a.ogg", "x.pdf"]),
        ?assertEqual({ok, ["a.ogg"]}, search:audio_files(T))
     end}.

test_audio_files_with_sequenced_ogg(T) ->
    {".ogg files are returned",
     fun() ->
        setup(T, [], ["002-a.ogg @ a.ogg", "x.pdf"]),
        ?assertEqual({ok, ["002-a.ogg"]}, search:audio_files(T))
     end}.

test_audio_files_with_flac(T) ->
    {".flac files are returned",
     fun() ->
        setup(T, [], ["a.flac", "x.pdf"]),
        ?assertEqual({ok, ["a.flac"]}, search:audio_files(T))
     end}.

test_audio_files_with_sequenced_flac(T) ->
    {".flac files are returned",
     fun() ->
        setup(T, [], ["01-a.flac @ a.flac", "x.pdf"]),
        ?assertEqual({ok, ["01-a.flac"]}, search:audio_files(T))
     end}.

test_audio_files_with_mpa(T) ->
    {".mpa files are returned",
     fun() ->
        setup(T, [], ["a.mpa", "x.pdf"]),
        ?assertEqual({ok, ["a.mpa"]}, search:audio_files(T))
     end}.

test_audio_files_with_sequenced_mpa(T) ->
    {".mpa files are returned",
     fun() ->
        setup(T, [], ["0003-a.mpa @ a.mpa", "x.pdf"]),
        ?assertEqual({ok, ["0003-a.mpa"]}, search:audio_files(T))
     end}.

test_audio_files_with_multiple_same_kind(T) ->
    {"All files with matching names are returned",
     fun() ->
        Fs = ["a.mp3", "x.mp3"],
        setup(T, [], Fs),
        ?assertEqual({ok, Fs}, search:audio_files(T))
     end}.

test_audio_files_with_multiple_different_kind(T) ->
    {"All files with matching names are returned, irrespetive of extension.",
     fun() ->
        setup(T, [], ["a.mp3", "x.ogg", "c.pdf"]),
        ?assertEqual({ok, ["a.mp3", "x.ogg"]}, search:audio_files(T))
     end}.

test_audio_files_with_matching_dirs(T) ->
    {"Directories with matching names are ignored.",
     fun() ->
        setup(T, ["a.mp3", "x.ogg", "c.pdf"], []),
        ?assertEqual({ok, []}, search:audio_files(T))
     end}.

test_audio_files_with_matching_files_and_dirs(T) ->
    {"Only files with matching names are returned.",
     fun() ->
        setup(T, ["a.mp3", "x.ogg", "c.pdf"], ["b.mp3", "c.pdf"]),
        ?assertEqual({ok, ["b.mp3"]}, search:audio_files(T))
     end}.

test_audio_files_with_non_existent_path(_) ->
    {"An error is returned for non-existent directories.",
     fun() ->
        ?assertMatch({error,_}, search:audio_files("/xxx"))
     end}.

test_audio_files_with_non_directory_path(T) ->
    {"An error is returned when the argument passed in not a directory path.",
     fun() ->
        {_, _, [FP]} = setup(T, [], ["x.pdf"]),
        ?assertMatch({error,_}, search:audio_files(FP))
     end}.

test_audio_files_with_non_writable_directory(T) ->
    {"An error is returned when the directory path is not read/write.",
     fun() ->
        setup(T, [], []),
        % Make T read-only for owner only.
        file:change_mode(T, 8#00400),
        ?assertMatch({error,_}, search:audio_files(T))
     end}.

test_audio_files_with_multiple_sorts_oldest_to_newest(T) ->
    {"All files with matching names are returned",
     fun() ->
        Fs = ["a.mp3", "b.mp3", "x.mp3"],
        {_, _, FQFs} = setup(T, [], Fs),
        lists:foreach(
            fun({F, D}) -> file:change_time(F, {{2010, 11, D},{0,0,0}}) end,
            lists:zip(FQFs, [25, 15, 5])),
        ?assertEqual({ok, lists:reverse(Fs)}, search:audio_files(T))
     end}.

test_audio_files_with_equal_mtime_sorts_on_filename(T) ->
    {"All files with matching names are returned",
     fun() ->
        Fs = ["a.mp3", "b.mp3", "x.mp3"],
        {_, _, FQFs} = setup(T, [], Fs),
        lists:foreach(
            fun({F, D}) -> file:change_time(F, {{2010, 11, D},{0,0,0}}) end,
            lists:zip(FQFs, [25, 25, 25])),
        ?assertEqual({ok, Fs}, search:audio_files(T))
     end}.

