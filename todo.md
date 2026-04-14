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

If yakuman detected, skip the questions about waits, riichi etc.
However, the getYaku function requires most of the question answers to be run.
Would need to split it into separate yakuman and regular yaku functions, and run
the Yakuman check early.

We now deduce hand closure from riichi, tsumo, damaten and number of open melds,
so we can account for ron not opening a hand but opening a meld.

We don't yet distinguish between yaku and han - a hand with no yaku, only dora,
is treated like normal and has its score printed.

The getYaku and getFu functions have grown too complicated in their signatures.
Moreover, they are not very interoperable / useful outside of the context of
printing to the user as we do right now. Need to add a data type that tracks a
"Scored hand", which contains the Hand data, InterpretedHand or chiittoitsu
data, ryanman wait, riichi, tsumo, ippatsu etc etc
The we can instantiate such a type and just pass that to the getYaku and getFu
functions.
Moreover, the yaku a hand has should be encoded in the type system, not just
returned alongside a han value as a string of output for the user as it is now.
Need to separate these two functions - getting the yaku of a hand, and
displaying those yaku in the CLI. Furthermore, many functions performed by the 
display file really ought to be part of the CLI but not the Library.
Major refactors are needed. As of now we just have a working prototype.
