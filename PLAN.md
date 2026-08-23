# Galaxy World — architecture & plan

Status: living document, first cut 2026-08-23 (the architecting
session). Companion org record: `future.org` → "Galaxy World" entry,
pts 1–8. Repo scaffolded same session.

## What this is

Gornskew's flagship: the game the Basilisk-class ships sail in.
Several basilisk stacks on the internet compete in various challenges
(survive-the-era flavor, Three-Body-Problem-adjacent: civilizations
under orbital mechanics they don't control). The basilisk is the
first seed prototype ship class; the yard
(`gitlab.genworks.com/gornskew/basilisk`) builds ships, this repo is
the world they sail in.

## The architecture decision: sovereign ships, thin beacon

The open question was centralized services vs pure peer-to-peer.
Decision: **neither pole** —

1. **Ships are authoritative over themselves.** A ship's state
   (herds, courses, logs, era outcomes) lives aboard. There is no
   central game-state database, nothing that must scale with player
   count, nothing whose hosting bill grows with success.
2. **Ship-to-ship is a narrow hail protocol.** Signed structured
   messages (identify, view-summary, challenge-attest), terminated at
   the ship's cyclops edge. *Data, never code* — the lisply eval
   surfaces stay in-network, the same discipline that keeps a
   basilisk off the open internet today. Specs incubate in
   `protocol/` before any implementation.
3. **Three thin centralized conveniences**, none of them a game-state
   authority:
   - **The lens** — galaxyworld.space, the web face. Serves the
     first-light view; fronts a reference ship for visitors who have
     no ship of their own. The web is centralized by nature; nothing
     else needs to be.
   - **The beacon** — discovery only: ship name, public key,
     reachable endpoint. Starts as a signed roster; can grow into a
     rendezvous/NAT relay if real galaxies need one. Third-party
     beacons are possible by construction (a galaxy is a roster of
     ships that exchanged keys, not an account on our server).
   - **Era issuance** — challenges published as signed content; ships
     verify and weather them locally; outcomes are signed
     attestations exchanged ship-to-ship or shown to the lens.
     Integrity at hobby scale = attestation + reputation, not
     server-side anti-cheat; revisit only if ranked/paid play arrives.

Why this fits: the free tier is self-hosted by design (git clone +
`./basilisk up`), so gameplay must not depend on our uptime; the paid
tier (hosted ships on Gornskew infra, per the standing Lisply-service
phases) plugs in unchanged — a hosted ship is just a ship berthed
ashore; and the lore already says ships, not empires.

Consequence for the pronto item: **first light needs none of the
protocol.** One ship plus the lens. Beacon/hails/eras stay design
documents until a galaxy holds more than one ship.

## The pieces

| piece | where |
|---|---|
| The solver (done, fleet-verified 9/9 ×4 berths) | `gw/apps/chartroom`: `:bsk-astro` CFFI over `libbskastro.so` + `:chartroom` Gendl face + the Navigator's bridge at `chartroom:9110` |
| The world model & the lens | `source/` here — `:galaxy-world` Gendl codebase, `:depends-on (:chartroom)` |
| Protocol drafts (hails, beacon, eras) | `protocol/` here — specs only, no code yet |
| The ship class | the basilisk yard repo (not here; this repo never absorbs the yard) |

Note the seam: chartroom is a Genworks-side app consumed by a
Gornskew product — the VAR line working as intended.

## Layer map (first light)

```
 visitor's browser:  x3dom scene (two eye viewpoints) + plot form
 --------------------------------------------------------------
 cyclops edge:       galaxyworld.space vhost, fixed URL whitelist
 --------------------------------------------------------------
 source/             bridge-view (embedded-x3dom-world sheet)
                     plotter (form -> validated params -> course)
                     catalog (destinations: bodies, mu, elements)
                     assembly (site root, vhost publish)
 --------------------------------------------------------------
 :chartroom          orbit / hohmann-transfer / assembly
                     (orbits are real Gendl curves -> x3d emission
                      rides the standard pipeline; verified live
                      2026-08-23: cad-output-tree through the x3d
                      format on a chartroom assembly, no new code)
 --------------------------------------------------------------
 :bsk-astro (0.17us in-image)  |  Navigator hail (846us+, full sim)
                the reckon-vs-hail seam stays visible
```

## Work plan — GW-1 "first light" (the pronto deliverable)

- [x] GW-0 Scaffold: repo, README (lore register), this plan,
      `:galaxy-world` system skeleton loading against `:chartroom`
- [ ] GW-1a The scene: `bridge-view` — home system from the catalog
      via chartroom geometry, background starfield, **two x3dom
      Viewpoints = the port and starboard yellow eyes**, eye-to-eye
      toggle on the page
- [ ] GW-1b The plot: destination catalog (solar-system bodies
      first; stars as background until they're destinations), plot
      form (destination, departure, transfer kind) → in-image
      solver → transfer curve rendered into the same scene with a
      legible summary (delta-v, time of flight)
- [ ] GW-1c The face & the posture: landing page, vhost publish for
      galaxyworld.space, view-and-plot-only audit (below), cyclops
      whitelist entry
- [ ] GW-1d Hand off for deploy: clean commits; DNS + edge vhost +
      backend load are the deploy owner's (galaxyworld.dev waits on
      edge TLS — HSTS-preloaded TLD)

## Security posture: view and plot only

The public face exposes **no eval surface, ever**:

- Only fixed, published URLs on the vhost; no geysr/tasty/ta2 or any
  development route reachable through it.
- Form input is parsed as numbers and catalog enums, never read as
  Lisp; unknown fields dropped.
- Solver calls from the public path are in-image (`:bsk-astro`,
  microseconds, no queue to flood). Full Navigator simulations are
  NOT triggered by anonymous visitors until rate-limiting exists —
  canned sim results are fine for first light.
- The ship's own lisply/MCP surfaces stay in-network, unchanged.

## Deferred (recorded so it isn't lost)

- In-world rendering/texturing of the craft themselves — **trigger:
  a galaxy holds more than one ship, or mirrors are discovered.**
  Until then the eyes look out, and nothing looks back.
- Herd health/growth as a game metric (herd canon deliberately
  early-sketch; see the universe-refinements org entry).
- Beacon, hail protocol, era engine implementations (`protocol/`
  drafts first).
- Steam / Screeps-style thin client; itch.io listing and the rest of
  the distribution plan (org entry pt 1).

## Open questions (flag if wrongly decided)

- Does the reference ship the lens fronts run on the existing site
  backend, or earn its own berth? First light: existing backend,
  same pattern as the other properties.
- Eye placement/geometry: until the craft likenesses settle
  (`basilisk/artwork/`), the two eyes are two viewpoints at a
  plausible stance — port and starboard of the bow, looking
  outboard-forward. Revisit when the class gets its in-world body.
- Attestation format (what a ship signs about an era outcome) —
  design with the first real challenge, not before.
