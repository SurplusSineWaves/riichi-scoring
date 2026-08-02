Yaku we ignore: (Blessing of heaven, robbing a kan etc)

Tenhou 
Double Riichi 
Haitei 
Houtei 
Rinshan Kaihou 
Chankan 
Chiihou 
Renhou 

Yaku done:

Riichi 
San Ankou 
San Kantsu      
Honitsu 
Chinitsu 
San Shoku Doujun 
San Shoku Doukou 
Tanyao 
Kokushi Musou 
Chii Toitsu 
Toitoi 
Dai Sangen 
Dai Suu Shii 
Shou Sangen 
Shou Suu Shii 
Ryanpeikou 
Iipeikou 
Yakuhai 
Chanta 
Ittsuu 
Honroutou 
Junchan 
Tsuu Iisou 
Chinroutou 
Ryuu Iisou 
Chuuren Poutou 
Suu Kantsu 
Menzen Tsumo 
Ippatsu 
Pinfu 

Instead of asking about the wait type of the hand, we could just ask for the
winning tile and work it out ourselves. This would make the user experience a
bit more straightforward.
Would need functions to tell if a partial chi has an open or closed wait and so 
on. We already know how to get the waits of a tenpai hand. With a bit more
structure added to the data, we could figure out what sort of waits the hand has
too.
Target:
    If the wait is needed (for Fu calculation for example), ask the user for the
    winning tile. Subtract this tile from the hand. Getting the waits normally
    considers all interpretations of the hand, but now we know for sure which
    interpretation we are looking at already. Thus can be more specific about
    the waits and what sort of wait it is. (Recall that some hands might have an
    open wait in one interpretation, and something else in another)

Add logic for Shanten calculation, and support for basic "tile efficiency"
strategy calculations (given a hand, which discard leaves the most chances to
reduce shanten on the next draw?).

Note that it is possible that interpreting a hand with more taatsu and less
melds can give lower shanten. It could happen, then, that a hand has called a
meld - so that meld is fixed - but out algorithm for shanten calculation won't
fix this meld and might find lower shanten by interpreting the hand without the
meld. Ideally this should be avoided. In practice I feel this isn't worth
worrying about though, for now at least. Even if the hand has more than 14
tiles, so that a Kan is necessarily present, we'll permit ourselves to consider
that Kan's tiles as forming Taatsu. Is there a case in which this gives shanten
less than -1? Yes! With some Kans, you can get interpretations where the number
of melds and taatsu / pairs exceeds 5! That definitely does need to be fixed...
Also, some more pruning of the search tree is required, things need to be
optimised. I notice when calculating shanten of tricky hands with many Kans, the
combinatorics can blow up too much and the program slows to a crawl..

Optimisations made, comments added. Now considering whether formPartials should
be passed an int so it can limit recursion depth (don't find more than x
partials since we already found 5-x melds).
