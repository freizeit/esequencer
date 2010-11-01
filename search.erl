-module(search).
-export([audio_files/1]).


af_filter(F, A) ->
    case util:is_link(F) of
        true -> A;
        false -> [filename:basename(F)|A]
    end.

% -----------------------------------------------------------------------------
%% @doc Search for audio files in the path given. The following extensions
%% are considered: (flac|ogg|mp3|mpa).
-spec audio_files(Path :: string()) -> {ok, [string()]} | {error, string()}.
audio_files(Path) ->
    case file:read_link_info(Path) of
        {error, E} ->
            {error, Path ++ " is broken (" ++ atom_to_list(E) ++ ")"};
        {ok, FIT} ->
            Ok = element(3,FIT) =:= directory andalso
                 element(4,FIT) =:= read_write,
            if
                Ok ->
                   Regex = "\.(flac|ogg|mp3|mpa)$",
                    Fs = filelib:fold_files(
                        Path, Regex, false, fun af_filter/2, []),
                    {ok, lists:sort(Fs)};
                not Ok ->
                    {error, Path ++ " is not a directory or wrong permissions"}
            end
    end.
