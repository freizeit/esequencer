-module(util_tests).

-include_lib("eunit/include/eunit.hrl").

-import(helpers, [setup/2, setup/3]).
-import(util).


% -----------------------------------------------------------------------------
max_sequence_number_with_empty_files_list_test() ->
    ?assertEqual(0, util:max_sequence_number([])).

max_sequence_number_with_all_sequenced_test() ->
    ?assertEqual(9, util:max_sequence_number(["002-a", "01-b", "9-c"])).

max_sequence_number_with_all_mixed_test() ->
    ?assertEqual(9, util:max_sequence_number(["a-a", "01-b", "9-c"])).

max_sequence_number_with_all_unsequenced_test() ->
    ?assertEqual(0, util:max_sequence_number(["a-a", "b-b"])).

max_sequence_number_with_digits_but_no_dash_test() ->
    ?assertEqual(0, util:max_sequence_number(["001a"])).


% -----------------------------------------------------------------------------
target_file_name_with_seqnum_too_wide_test() ->
    ?assertEqual(
        {esnfmt, "    ! a (esnfmt)"}, util:target_file_name("a", 333, 2)).

target_file_name_with_seqnum_as_wide_as_width_test() ->
    ?assertEqual(
        {ok, "987-a"}, util:target_file_name("a", 987, 3)).

target_file_name_with_seqnum_less_wide_than_width_test() ->
    ?assertEqual(
        {ok, "00000987-a"}, util:target_file_name("a", 987, 8)).


% -----------------------------------------------------------------------------
sequence_file_test_() ->
    {foreach,
     fun() -> setup([], ["a", "x", "00001-x"]) end,
     fun({T, _, _}) -> ?cmd("rm -rf " ++ T) end,
     [
        fun test_sequence_file_with_set_dryrun_flag/1,
        fun test_sequence_file_result_with_set_dryrun_flag/1,
        fun test_sequence_file_result_with_unset_dryrun_flag/1,
        fun test_sequence_file_result_with_no_source/1,
        fun test_sequence_file_result_with_existing_target/1,
        fun test_sequence_file_result_with_seqnum_too_wide/1,
        fun test_sequence_file/1
     ]
    }.

test_sequence_file_with_set_dryrun_flag({T, _, _}) ->
    {"sequence_file() observes the dry-run flag.",
     fun() ->
        util:sequence_file(T, "a", 1, true, 5),
        ?assert(filelib:is_regular(filename:join(T, "a"))),
        ?assert(not filelib:is_file(filename:join(T, "00001-a")))
     end}.

test_sequence_file_result_with_set_dryrun_flag({T, _, _}) ->
    {"A string detailing what would be done is returned.",
     fun() ->
        {ok, M} = util:sequence_file(T, "a", 1, true, 5),
        ?assertEqual("    . a -> 00001-a", M)
     end}.

test_sequence_file_result_with_unset_dryrun_flag({T, _, _}) ->
    {"A string detailing what was done is returned.",
     fun() ->
        {ok, M} = util:sequence_file(T, "a", 1, false, 5),
        ?assertEqual("    * a -> 00001-a", M)
     end}.

test_sequence_file_result_with_no_source({T, _, _}) ->
    {"Test sequence_file() with missing source file.",
     fun() ->
        {enoent, M} = util:sequence_file(T, "b", 1, false, 5),
        ?assertEqual("    ! b (enoent)", M)
     end}.

test_sequence_file_result_with_existing_target({T, _, _}) ->
    {"Test sequence_file() with missing permissions.",
     fun() ->
        ?assert(filelib:is_file(filename:join(T, "00001-x"))),
        {eexist, M} = util:sequence_file(T, "x", 1, false, 5),
        ?assertEqual("    ! x (eexist)", M)
     end}.

test_sequence_file_result_with_seqnum_too_wide({T, _, _}) ->
    {"Test sequence_file() with a too wide sequence number.",
     fun() ->
        {esnfmt, M} = util:sequence_file(T, "a", 9999, false, 3),
        ?assertEqual("    ! a (esnfmt)", M)
     end}.

test_sequence_file({T, _, _}) ->
    {"The audio file is renamed, a symbolic link with the old name is created",
     fun() ->
        util:sequence_file(T, "a", 1, false, 5),
        ?assert(filelib:is_file(filename:join(T, "a"))),
        ?assert(util:is_link(filename:join(T, "a"))),
        ?assert(filelib:is_regular(filename:join(T, "00001-a")))
     end}.


% -----------------------------------------------------------------------------
is_link_test_() ->
    {foreach,
     fun() ->
        {T, _, Fs} = setup([], ["a"]),
        {T, hd(Fs)}
     end,
     fun({T, _}) -> ?cmd("rm -rf " ++ T) end,
     [
        fun test_is_link_with_link/1,
        fun test_is_link_with_regular/1,
        fun test_is_link_with_non_existing/1
     ]
    }.

test_is_link_with_regular({_, P}) ->
    {"is_link() returns false for regular files.",
     fun() ->
        ?assert(not util:is_link(P))
     end}.

test_is_link_with_link({_, P}) ->
    {"is_link() returns true for symbolic links.",
     fun() ->
        os:cmd(io_lib:format("ln -s ~s ~s.link", [P, P])),
        ?assert(util:is_link(P ++ ".link"))
     end}.

test_is_link_with_non_existing({_, P}) ->
    {"is_link() returns false for non-existent files.",
     fun() ->
        ?assert(not util:is_link(P ++ ".link"))
     end}.
