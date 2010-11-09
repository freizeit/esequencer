-module(search).
-export([audio_files/1]).


% -----------------------------------------------------------------------------
%% @doc Search for audio files in the path given. The following extensions
%% are considered: (flac|ogg|mp3|mpa). The maximum sequence number in use
%% is calculated in the same pass. Also, the already sequenced files are
%% filtered.
-spec audio_files(Path :: string()) -> {ok, [string()]} | {error, string()}.
audio_files(Path) ->
    case file:read_link_info(Path) of
        {error, E} ->
            {error, Path ++ " is broken (" ++ atom_to_list(E) ++ ")"};
        {ok, FIT} ->
            %% Is this a directory for which we have read/write access?
            Ok = element(3,FIT) =:= directory andalso
                 element(4,FIT) =:= read_write,
            if
                Ok -> search_files(Path);
                not Ok ->
                    {error, Path ++ " is not a directory or wrong permissions"}
            end
    end.


search_files(Path) ->
    % Compile the regex once only.
    {ok, Seqnum_regex} = re:compile("^(?<SEQNUM>[0-9]+)-"),
    Match_sequenced_file = fun(Fn) ->
        re:run(Fn, Seqnum_regex, [{capture, ['SEQNUM'], list}])
        end,
    AF_filter = fun(F, {M, A}) ->
        case util:is_link(F) of
            true -> {M, A}; % Ignore symbolic links.
            false -> % Regular file here.
                Fn = filename:basename(F),
                % Is file already sequenced?
                case Match_sequenced_file(Fn) of
                   % Yes, pick up the higher sequence number.
                    {match, [S]} -> {max(list_to_integer(S), M), A};
                    nomatch -> {M, [Fn|A]} % No, pick up the file.
                end
        end
    end,
    % Find the unsequenced audio files as well as the highest sequence
    % number currently in use.
    {Max_seqnum, Fs} = filelib:fold_files(
        Path, "\.(flac|ogg|mp3|mpa)$", false, AF_filter, {0, []}),
    {ok, {Max_seqnum, Fs}}.
