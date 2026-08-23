# The galaxy protocol — drafts only

Nothing in here runs. These are the design documents for the day a
galaxy holds more than one ship:

- **hails** — ship-to-ship messages: identify, view-summary,
  challenge-attest. Signed, structured, capability-scoped, terminated
  at the ship's edge. Data, never code.
- **beacon** — discovery: name, public key, endpoint. A galaxy is a
  roster of ships that exchanged keys; anyone can keep a beacon.
- **eras** — challenges as signed content; outcomes as signed
  attestations.

The decision record behind this shape (sovereign ships, thin beacon)
is in PLAN.md at the repo root.
