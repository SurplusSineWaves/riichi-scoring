This package provides a CLI tool with commands for determining the yaku, fu, and score of a hand in Riichi Mahjong. It can also determine the waits of a partial hand,
determine shanten, and tile efficient discards.

The CLI tool supplied is the riichi command. Help information is as follows:

        riichi <subcommand> "<hand>"

        Subcommands:
                yaku      Determine yaku of a completed hand.
                waits     Determine the waits of a ready hand.
                score     Score a completed hand.
                shanten   Get the shanten of a hand.
                discard   Determine which discard for a hand has the best
                          tile efficiency.
                live      Interactive mode. Give discard recommendations for
                          a hand until it is complete.

        "yaku", "score", "discard" and "live" subcommands expect a full hand.
        "waits" subcommand expects a hand that is tenpai.

        Example hands include:
                "123p 234m 444p rrrr NN"
                "344556s 444p 222m EE"
                "19p 19s 19m 1p NESWrgw"

        In detail, numeric tiles are denoted (1-9) + (m, p, or s),
        Winds are denoted N, E, S, W, and Dragons are r, w, g.
        A 0 can be used to denote a red five.
        Numeric tiles of the same suit, and honour tiles, can be
        grouped as seen in the examples (but needn't be).

        In scoring a hand, dora and seat/round wind may be required,
        also supplied in this format.

Internal libraries are also exposed.

Hackage page: https://hackage.haskell.org/package/riichi-scoring
