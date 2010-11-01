-module(util).
-export([max_sequence_number/1, target_file_name/3, concats/1, is_link/1]).
-export([sequence_file/5, sequence_files/5]).
-include_lib("eunit/include/eunit.hrl").


%% @doc Return the highest sequence number in use for a given list of audio
%% file names.
-spec max_sequence_number(Filenames :: [string()]) -> integer().
max_sequence_number(Filenames) ->
    {ok, Seqnum_regex} = re:compile("^(?<SEQNUM>[0-9]+)-"),
    %% Capture the 'SEQNUM' group as a string.
    Options = [{capture, ['SEQNUM'], list}],
    Ms = lists:map(fun(N) -> re:run(N, Seqnum_regex, Options) end, Filenames),
    Ss = [S || {match, [S]} <- lists:filter(fun(M) -> M =/= nomatch end, Ms)],
    case Ss of
        [] -> 0;  %% None of the files was sequenced.
        _ -> lists:max([list_to_integer(S) || S <- Ss])
    end.


%% @doc Prefixes F with "SN-", SN is padded with zeroes from the left if less wide than Width.
-spec target_file_name(F :: string(), SN :: integer(), Width :: integer()) -> {atom(), string()}.
target_file_name(F, SN, Width) ->
    case string:len(integer_to_list(SN)) > Width of
        true -> fmt_error(esnfmt, F);
        false ->
            W = integer_to_list(Width),
            Format = concats(["~", W, ".", W, ".0w"]),
            V =  lists:flatten(io_lib:format(Format, [SN])),
            {ok, concats([V, "-", F])}
    end.


%% @doc Concatenate a list of strings.
-spec concats(L :: [string()]) -> string().
concats(L) -> lists:foldr(fun string:concat/2, "", L).


%% @doc Format a success message for sequence_file().
-spec fmt_success(Prefix :: string(), S :: string(), T :: string()) -> {ok, string()}.
fmt_success(Prefix, S, T) ->
    M = concats(["    ", Prefix, " ", S, " -> ", T]),
    {ok, M}.


%% @doc Format an error message for sequence_file().
-spec fmt_error(Error :: atom(), F :: string()) -> {atom(), string()}.
fmt_error(Error, F) ->
    {Error,  concats(["    ! ", F, " (", atom_to_list(Error), ")"])}.

    
%% @doc Rename F to SN-F and create a symbolic link F pointing to SN-F.
-spec sequence_file(D :: string(), F :: string(), SN :: integer(), Dryrun :: boolean(), Width :: integer()) -> {atom(), string()}.

sequence_file(_, F, SN, true, Width) ->
    %% This is the "dry run" version, it does not perform any actions.
    case target_file_name(F, SN, Width) of
        {ok, TargetFile} -> fmt_success(".", F, TargetFile);
        {Err, _} -> fmt_error(Err, F)
    end;

sequence_file(D, F, SN, false, Width) ->
    case target_file_name(F, SN, Width) of
        {ok, TargetFile} ->
            %% Target file name formatting succeeded.
            Source = filename:join(D, F),
            Target = filename:join(D, TargetFile),
            case filelib:is_regular(Target) of
                true -> fmt_error(eexist, F);
                false ->
                    %% Target file name does not exist, go ahead with renaming.
                    case file:rename(Source, Target) of
                        ok ->
                            %% File renamed, create a symlink with old name.
                            case file:make_symlink(TargetFile, Source) of
                                ok -> fmt_success("*", F, TargetFile);
                                {error, Err} -> fmt_error(Err, F)
                            end;
                        {error, Err} -> fmt_error(Err, F)
                    end
            end;
        {Err, _} -> fmt_error(Err, F)
    end.


sequence_files(D, Fs, SN, Dryrun, Width) ->
    sequence_files(D, Fs, SN, Dryrun, Width, []).


sequence_files(_, [], _, _, _, A) -> lists:reverse(A);
sequence_files(D, [F|Fs], SN, Dryrun, Width, A) ->
    R = sequence_file(D, F, SN, Dryrun, Width),
    case R of
        % Abort in case of a sequence number overflow, since there's no point
        % in carrying on.
        {esnfmt, _} -> sequence_files(D, [], SN+1, Dryrun, Width, [R|A]);
        _ -> sequence_files(D, Fs, SN+1, Dryrun, Width, [R|A])
    end.


%% @doc Return true if the path P is a symbolic link, false otherwise.
-spec is_link(P :: string()) -> boolean().
is_link(P) ->
    case file:read_link_info(P) of
        {ok, FIT} -> element(3,FIT) =:= symlink;
        {error, _} -> false
    end.
