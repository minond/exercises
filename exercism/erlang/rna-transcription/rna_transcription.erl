-module(rna_transcription).
-export([to_rna/1]).

get_complement(Nucleotide) ->
    maps:get(Nucleotide, #{"G" => "C", "C" => "G", "T" => "A", "A" => "U"}).

to_rna(Sequence) ->
    string:join(
        lists:foreach(
            get_complement,
            %% string:tokens(Sequence)
            Sequence
        ), ""
    ).
