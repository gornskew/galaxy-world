# Galaxy World

This is a game where you get a ship and fly it around in a galaxy.

The game is currently single-player but envisions a multi-player
future. So for now you can just explore around the (synthetic) galaxy,
or compete in various challenges that may or may not appear. The
Basilisk is the first seed prototype ship class; the yard
([github.com/gornskew/basilisk](https://github.com/gornskew/basilisk))
builds ships in its vat halls and its scroll chest contains the
needed details. This scroll chest sets up the galaxy they float in.

A Basilisk is a great yellow-eyed dirigible-shaped vessel — a ship
that is grown, crewed, and sailed; the yard's own scrolls tell that
story.

## First light (under construction)

Landing at galaxyworld.space puts you in the cockpit, hands on the
wheel: home in orbit abeam to port, continents filing past the
glass, stars wheeling past the bubble roof. One deck up, the bridge:
the view out your basilisk's two side eyes — each yellow eye a
camera — and a chart table for plotting a course to a star or
planet, worked by the ship's own chartroom and rendered so you can
see the road you would sail. View, plot, and drive; the helm answers
to no one ashore.

## Some Foundations

1. **Ships are authoritative over themselves.** A ship's state
   (courses, logs, era outcomes) lives aboard. There is no
   central game-state database.
2. **Ship-to-ship is a narrow hail protocol.** Signed structured
   messages (identify, view-summary, challenge-attest), terminated at
   the ship's transporter room. Words, never workings — and words on
   paper only, until a galaxy holds more than one ship.
3. **A few things live ashore, and your ship depends on none of
   them:**
   - **The lens** — galaxyworld.space, the game's window on the
     web. For visitors with no ship of their own, it looks out from
     a reference ship.
   - **The beacon** — a signpost for ships that want to be found:
     name, public key, where to hail. A galaxy is a roster of ships
     that have exchanged keys, not an account on a server — anyone
     can keep a beacon.
   - **Eras** — challenges arrive as sealed, signed scrolls any ship
     can verify. Every ship weathers an era aboard, under the same
     sky, and what he attests about the outcome is his to sign.

Your ship never needs anyone's servers to sail. The pieces ashore
are conveniences, not authorities: the game works when the lens is
dark.


## Getting a ship

The yard is at
[github.com/gornskew/basilisk](https://github.com/gornskew/basilisk).
A ship of your own is a clone and a raising: `./basilisk up`.

## License

AGPL-3.0-or-later, © 2026 Gornskew Enterprises. See `LICENSE`.
