-module(helpers).
-export([setup/2, setup/3, teardown/1]).
-include_lib("eunit/include/eunit.hrl").


%% @doc Test helper function, sets up a temporary directory and creates the given directories and files in it.
setup(Ds, Fs) ->
    T = string:strip(os:cmd("mktemp -d"), right, $\n),
    setup(T, Ds, Fs).


setup(T, Ds, Fs) ->
    {ok, File_regex} = re:compile(
        "^(?<FILE>[^@]+?)(:?\s*@\s*(?<SYMLINK>[^@]+))?$"),
    FQDs = lists:map(fun(D) -> filename:join(T, D) end, Ds),
    lists:foreach(fun(D) -> os:cmd("mkdir -p " ++ D) end, FQDs),
    FQFs = lists:map(fun(F) -> setup_file(T, F, File_regex) end, Fs),
    {T, FQDs, FQFs}.

%% @doc Test helper function, removes the given directory and any sub-directories or files it contains.
teardown(T) -> os:cmd("rm -rf " ++ T).


% -----------------------------------------------------------------------------
%% @doc Creates a file in T, if FE ~ "F @ S" a symbolic link to the file is created as well. Returns the absolute path of the file created.
-spec setup_file(T :: string(), FE :: string(), Regex :: re:mp()) -> string().
setup_file(T, FE, Regex) ->
    %% Capture the regex patterns as strings.
    Options = [{capture, ["FILE", "SYMLINK"], list}],
    case re:run(FE, Regex, Options) of
        nomatch -> throw({nomatch, FE});
        {match, [F, []]} ->
            % No symbolic link name found, just create the file and be done.
            P = filename:join(T, F),
            os:cmd("touch " ++ P),
            P;
        {match, [F, S]} ->
            % Symbolic link name found, create the file and the link.
            FP = filename:join(T, F),
            LP = filename:join(T, S),
            os:cmd("touch " ++ FP),
            os:cmd(io_lib:format("ln -s ~s ~s", [F, LP])),
            FP
    end.
    

% -----------------------------------------------------------------------------
%% Tests for setup() %%
setup_creates_directories_test() ->
    Ds = ["a", "x"],
    {T, _, _} = setup(Ds, []),
    lists:foreach(
        fun(D) -> ?assert(filelib:is_dir(filename:join(T,D))) end, Ds),
    os:cmd("rm -rf " ++ T).

setup_creates_files_test() ->
    Fs = ["a", "x"],
    {T, _, _} = setup([], Fs),
    lists:foreach(
        fun(F) -> ?assert(filelib:is_regular(filename:join(T,F))) end, Fs),
    os:cmd("rm -rf " ++ T).

setup_creates_files_and_directories_test() ->
    Ds = ["a", "b"],
    Fs = ["c", "d"],
    {T, _, _} = setup(Ds, Fs),
    lists:foreach(
        fun(F) -> ?assert(filelib:is_regular(filename:join(T,F))) end, Fs),
    lists:foreach(
        fun(D) -> ?assert(filelib:is_dir(filename:join(T,D))) end, Ds),
    os:cmd("rm -rf " ++ T).

setup_returns_fully_qualified_directories_test() ->
    Ds = ["a", "x"],
    {T, FQDs, _} = setup(Ds, []),
    Expected = lists:map(fun(S) -> filename:join(T, S) end, Ds),
    ?assertEqual(Expected, FQDs),
    os:cmd("rm -rf " ++ T).

setup_returns_fully_qualified_files_test() ->
    Fs = ["a", "x"],
    {T, _, FQFs} = setup([], Fs),
    Expected = lists:map(fun(S) -> filename:join(T, S) end, Fs),
    ?assertEqual(Expected, FQFs),
    os:cmd("rm -rf " ++ T).

setup_creates_files_and_links_test() ->
    Fs = ["a @ a.ln", "x @ x.ln"],
    Ns = ["a", "x"],
    {T, _, FQFs} = setup([], Fs),
    Expected = lists:map(fun(S) -> filename:join(T, S) end, Ns),
    ?assertEqual(Expected, FQFs),
    lists:foreach(
        fun(F) -> ?assert(filelib:is_regular(filename:join(T,F))) end, Ns),
    lists:foreach(
        fun(F) -> ?assert(util:is_link(filename:join(T,F++".ln"))) end, Ns),
    os:cmd("rm -rf " ++ T).

setup_creates_mixed_files_and_links_test() ->
    Fs = ["a", "x @ x.ln"],
    Ns = ["a", "x"],
    Ls = ["x"],
    {T, _, FQFs} = setup([], Fs),
    Expected = lists:map(fun(S) -> filename:join(T, S) end, Ns),
    ?assertEqual(Expected, FQFs),
    lists:foreach(
        fun(F) -> ?assert(filelib:is_regular(filename:join(T,F))) end, Ns),
    lists:foreach(
        fun(F) -> ?assert(util:is_link(filename:join(T,F++".ln"))) end, Ls),
    os:cmd("rm -rf " ++ T).


% -----------------------------------------------------------------------------
teardown_removes_directory_test() ->
    T = string:strip(os:cmd("mktemp -d"), right, $\n),
    teardown(T),
    ?assert(not filelib:is_file(T)).
