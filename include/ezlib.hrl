
-author("silviu.caragea").

%compression methods

-define(Z_DEFLATE, 1).
-define(Z_INFLATE, 2).

%compression levels

-define(Z_NO_COMPRESSION, 0).
-define(Z_BEST_SPEED, 1).
-define(Z_BEST_COMPRESSION, 9).
-define(Z_DEFAULT_COMPRESSION, -1).

%compression strategy

-define(Z_FILTERED,         1).
-define(Z_HUFFMAN_ONLY,     2).
-define(Z_RLE,              3).
-define(Z_FIXED,            4).
-define(Z_DEFAULT_STRATEGY, 0).

-type reason()                :: term().
-type ezlib_session()         :: reference().
-type compression_method()    :: ?Z_DEFLATE | ?Z_INFLATE.
-type compression_level()     :: ?Z_NO_COMPRESSION | ?Z_BEST_SPEED | ?Z_BEST_COMPRESSION | ?Z_DEFAULT_COMPRESSION.
-type compression_strategy()  :: ?Z_FILTERED | ?Z_HUFFMAN_ONLY | ?Z_RLE | ?Z_FIXED | ?Z_DEFAULT_STRATEGY.

-type ezlib_option()::
    {compression_level, compression_level()} |
    {window_bits, non_neg_integer()} |
    {memory_level, non_neg_integer()} |
    {compression_strategy, compression_strategy()} |
    {use_iolist, boolean()}.