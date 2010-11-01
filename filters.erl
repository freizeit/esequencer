-module(filters).
-export([filter_paths/1, unsequenced_files/1]).
-include_lib("eunit/include/eunit.hrl").


%% @doc Return the file names that do not start with a "[0-9]+-" prefix.
-spec unsequenced_files(Filenames :: [string()]) -> [string()].
unsequenced_files(Filenames) ->
    {ok, Unseq_regex} = re:compile("^[0-9]+-"),
    Filter = fun(F) ->
        case re:run(F, Unseq_regex) of
            {match, _} -> false;
            _ -> true
        end
    end,
    lists:filter(Filter, Filenames).


%% @doc Return a list of strings which are valid directory paths.
-spec filter_paths(Paths :: [string()]) -> [string()].
filter_paths(Paths) -> filter_paths(Paths, []).

filter_paths([], R) -> lists:reverse(R);
filter_paths([P|Ps], R) ->
    case filelib:is_dir(P) andalso not lists:member(P, R) of
        true -> filter_paths(Ps, [P|R]);
        false -> filter_paths(Ps, R)
    end.
