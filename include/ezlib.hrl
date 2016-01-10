
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