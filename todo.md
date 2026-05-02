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

We don't yet distinguish between yaku and han - a hand with no yaku, only dora,
is treated like normal and has its score printed.

Potentially a good idea to replace some data objects with records, e.g Tile and
Meld.

A Mangan of less than 4 han is currently not labeled as such, but is scored
correctly.

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
