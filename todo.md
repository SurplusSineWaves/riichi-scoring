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
