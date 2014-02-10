%% @author Matthew Dempsky <matthew@mochimedia.com>
%% @copyright 2009-2014 Mochi Media, Inc.
%%
%% @doc Module for editing BEAM files.

-module(beambag_edit).
-export([swap/3]).

%% @doc Search through a BEAM file's literals table, replacing
%% instances of <code>From</code> with <code>To</code>.
-spec swap(Beam::binary(), From::term(), To::term()) ->
                  binary().
swap(Beam, From, To) ->
    enc([{Tag, sub(Tag, Chunk, From, To)} || {Tag, Chunk} <- dec(Beam)]).

sub(<<"LitT">>, Chunk, From, To) ->
    <<Size:32, Rest/bytes>> = Chunk,
    <<Count:32, LitT/bytes>> = zlib:uncompress(Rest),
    true = (4 + size(LitT) =:= Size), %% assert
    X = term_to_binary(To),
    NewLitT =
        <<<<(case binary_to_term(Lit) =:= From of
                 true ->
                     <<(size(X)):32, X/bytes>>;
                 false ->
                     <<N:32, Lit/bytes>>
             end)/bytes>> || <<N:32, Lit:N/bytes>> <= LitT>>,
    <<(4 + size(NewLitT)):32, (zlib:compress(<<Count:32, NewLitT/bytes>>))/bytes>>;
sub(_Tag, Chunk, _From, _To) ->
    Chunk.

dec(Bytes) ->
    <<"FOR1", Len:32, "BEAM", Body/bytes>> = Bytes,
    true = (4 + size(Body) =:= Len), %% assert
    dec(Body, []).

dec(<<Tag:4/bytes, Len:32, Rest1/bytes>>, Acc) ->
    Pad = pad4(Len),
    <<Chunk:Len/bytes, _Skip:Pad/bytes, Rest2/bytes>> = Rest1,
    dec(Rest2, [{Tag, Chunk} | Acc]);
dec(<<>>, Acc) ->
    lists:reverse(Acc).

enc(Chunks) ->
    Size = 4 + lists:sum([8 + size(Chunk) + pad4(size(Chunk))
                          || {_Tag, Chunk} <- Chunks]),
    enc(Chunks, <<"FOR1", Size:32, "BEAM">>).

enc([{Tag, Chunk} | Rest], Acc) ->
    Len = size(Chunk),
    Pad = pad4(Len),
    enc(Rest, <<Acc/bytes, Tag/bytes, Len:32, Chunk/bytes, 0:(Pad * 8)>>);
enc([], Acc) ->
    Acc.

pad4(N) ->
    -N band 3.
