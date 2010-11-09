-module(sequencer).
-export([sequence_paths/3, sequence_path/3, profile_paths/3]).
-include_lib("eunit/include/eunit.hrl").


profile_paths(Paths, Dryrun, Width) ->
    cprof:stop(),
    cprof:start(),
    sequence_paths(Paths, Dryrun, Width),
    cprof:pause().


%% @doc Search for unsequenced audio files in the paths given and sequence
%% them.
-spec sequence_paths(Paths :: [string()], Dryrun :: boolean(), Width :: integer()) -> atom().
sequence_paths([], _, _) -> ok;
sequence_paths([P|Paths], Dryrun, Width) ->
    {Path, Ms, Es} = sequence_path(P, Dryrun, Width),
    io:put_chars(["\n>> ", Path, "\n"]),
    lists:foreach(fun(S) -> io:put_chars([S, "\n"]) end, Ms),
    lists:foreach(fun(S) -> io:put_chars([S, "\n"]) end, Es),
    sequence_paths(Paths, Dryrun, Width).


%% @doc Search for unsequenced audio files in the path given and prefix
%% them with a numeric sequence of the given width.
-spec sequence_path(Path :: string(), Dryrun :: boolean(), Width :: integer()) -> {string(), [string()], [string()]}.
sequence_path(Path, Dryrun, Width) ->
    case search:audio_files(Path) of
        {error, E} -> {Path, [], [E]};
        {ok, []} -> {Path, ["    * Nothing to do."], []};
        {ok, {Max_seqnum, As}} ->
            case length(As) =:= 0 of
                true -> {Path, ["    * Nothing to do."], []};
                _ ->
                    Rs = util:sequence_files(
                        Path, As, Max_seqnum + 1, Dryrun, Width),
                    {Ms, Es} = format_results(Rs, [], []),
                    {Path, Ms, Es}
            end
    end.

format_results([], Ms, Es) -> {lists:reverse(Ms), lists:reverse(Es)};
format_results([R|Rs], Ms, Es) ->
    case R of
        {ok, M} -> format_results(Rs, [M|Ms], Es);
        {_, E} -> format_results(Rs, Ms, [E|Es])
    end.
