This package provides a CLI tool with commands for determining the yaku, fu, and score of a hand in Riichi Mahjong. It can also determine the waits of a partial hand.

The CLI tool supplied is the riichi command. Help information is as follows:

    Command riichi:
        Possible subcommands: yaku, waits, score (default = yaku)

    Usage:
        riichi <subcommand> "<hand>"

        "yaku" and "score" subcommands expect a full hand.
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

        In scoring a hand, dora and seat/round wind must be supplied,
        also in this format.

Internal libraries are also exposed.
