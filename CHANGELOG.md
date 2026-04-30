# Revision history for Mahjong

See git repo for more detailed commit information.

## 0.1.0.0
First functional prototype.

## 0.1.0.1
Dependency versions reworked.

## 0.2.0.0
Help messages improved. Module structure refactored. Documentation begun.

## 0.2.0.1
Revised cabal version in attempt to fix build error on Hackage. Build works
locally, suspect the problem is a quirk of Hackage build system.
Added some more documentation.

## 0.2.0.2
Internal library now unnamed. Hoping this fixes Hackage build fail.

## 0.2.0.3
Added docs for yaku functions.
Reworked seven pairs detection to ensure hand has 14 tiles.
9 gates now checks that the hand is closed.

## 0.3.0.0
Fixed error in scoring triple triplets, was previously scored as 1 han open.
Implemented new backend for scoring and yaku detection via "context" based
approach, defined in new Context submodule. This helps simplify function
signatures for operations that need many pieces of information besides just the
superficial composition of a hand.

## 0.3.1.0
Now only ask for dora after we know the hand is not a yakuman, as part of the
mkContext function (previously handled dora in displayHandScore).
Four concealed triplets now asks about concealment. It wasn't doing this before,
a bug introduced by the refactor, since mkYakumanContext intentionally gets less
information than mkYakuContext since generally it needs less.
Now only ask for wind context if we see a wind tile in the hand. Note even a
wind pair could matter - it won't affect most yaku but affects Fu.
Speaking of - 
    NOTE: Seat+Round wind pair counts as a yakuhai pair and awards
    2+2=4 Fu. Some rulesets would only award 2 Fu.

## 0.4.0.0
Extended documentation. Old getFu replaced with new one (previously _getFu).

