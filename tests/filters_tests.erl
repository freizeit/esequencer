-module(filters_tests).

-include_lib("eunit/include/eunit.hrl").

-import(helpers, [setup/2, setup/3]).
-import(filters).

% -----------------------------------------------------------------------------
filter_paths_test_() ->
    {foreach,
     fun() -> ?cmd("mktemp -d") -- "\n"  end,
     fun(T) -> ?cmd("rm -rf " ++ T) end,
     [
        fun test_filter_paths_files_only/1,
        fun test_filter_paths_directories_only/1,
        fun test_filter_paths_mixed/1,
        fun test_filter_paths_returns_unique/1
     ]
    }.
test_filter_paths_files_only(T) ->
    {"In the absence of any directories an empty list is returned.",
     fun() ->
        {_, _, FQFs} = setup(T, [], ["a", "x"]),
        ?assertEqual([], filters:filter_paths(FQFs))
     end}.

test_filter_paths_directories_only(T) ->
    {"All paths that point to directories are returned.",
     fun() ->
        {_, FQDs, _} = setup(T, ["tmp", "var"], []),
        ?assertEqual(FQDs, filters:filter_paths(FQDs))
     end}.

test_filter_paths_mixed(T) ->
    {"Only the paths that point to directories are returned.",
     fun() ->
        {_, FQDs, FQFs} = setup(T, ["a", "b"], ["c", "d"]),
        ?assertEqual(FQDs, filters:filter_paths(FQDs ++ FQFs))
     end}.

test_filter_paths_returns_unique(T) ->
    {"All paths returned are unique (no duplicates).",
     fun() ->
        {_, FQDs, FQFs} = setup(T, ["a", "b"], ["c", "d"]),
        ?assertEqual(FQDs, filters:filter_paths(FQDs ++ FQFs ++ FQDs))
     end}.


% -----------------------------------------------------------------------------
unsequenced_files_with_empty_files_list_test() ->
    ?assertEqual([], filters:unsequenced_files([])).

unsequenced_files_with_all_sequenced_test() ->
    ?assertEqual([], filters:unsequenced_files(["0-a", "01-b"])).

unsequenced_files_with_all_mixed_test() ->
    ?assertEqual(["a-a"], filters:unsequenced_files(["a-a", "01-b"])).

unsequenced_files_with_all_unsequenced_test() ->
    ?assertEqual(["a-a", "b-b"], filters:unsequenced_files(["a-a", "b-b"])).

unsequenced_files_with_digits_but_no_dash_test() ->
    ?assertEqual(["001a"], filters:unsequenced_files(["001a"])).
