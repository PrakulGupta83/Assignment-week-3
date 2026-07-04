# TableTuner — Smart Restaurant Recommender

A single-page web app for when you **can't decide where to order from or eat**.
It asks a few quick questions to read your mood, then names **one confident pick**
(with a plain-language "why"), a couple of backups, and — for ordering — keeps
everything tightly **around your current location**. Rate a place afterwards and
it quietly tunes future suggestions toward what you like.

## Features

- **Guided wizard**, in this order:
  1. **Veg / Non-veg** — a left↔right slider (middle = "either is fine")
  2. **Order in / Dine-in** — two clear buttons (not a slider)
  3. **Food type preference — ranked** — tap cuisines *in order of preference*
     (1st tap = top choice, 2nd = next…); each selected chip shows its rank
  4. **Ambience** — shown *only when you choose dine-in*
     (romantic, family, casual, lively, fine dining, outdoor, quiet)
  5. **Occasion** — everyday, **period comfort**, quick bite, friends, family,
     date, business, celebration
- **Period comfort mode** — picking *🌸 Period comfort* opens a quick craving
  shortcut (sweet / spicy / warm / cheesy / cold / hot drink), biases results
  toward comforting food, and keeps them **very nearby** so you don't have to
  step out.
- **Uses your exact current location** (device GPS via the Geolocation API) —
  not a city centre — to measure real distance and travel time. Decline it and
  you can pick a starting point instead.
- **Stays local, especially for ordering** — take-away (and period comfort)
  searches a tight radius and filters to places **within ~4 km**, so you're
  never sent 20 km across town. Dine-in allows more travel.
- **Real nearby restaurants** from the free OpenStreetMap
  [Overpass API](https://overpass-api.de/) — no API key. Falls back to a
  built-in sample dataset if the live lookup can't be reached.
- **One decisive recommendation** — a hero card with distance, travel/ready
  time, average bill, a match score, and a **Google Maps link**; two compact
  backups; and the full ranked list tucked behind a "See all nearby options"
  toggle.
- **Learns from your ratings** — the 1–5 stars you give a place are saved in
  your browser (`localStorage`) and adjust future rankings.

### A note on colours

The two-option controls deliberately avoid a red/green pairing. Red/green reads
as *right/wrong* or *go/stop*, and no answer here is "wrong" — so the app uses a
neutral **teal ↔ violet** pair for the sliders and a warm **marigold** accent
for actions.

## Running it

Static site — no build step.

```bash
# Option A: open directly
xdg-open index.html        # or `open index.html` on macOS

# Option B: serve it (some browsers require this for geolocation)
python3 -m http.server 8000
# then visit http://localhost:8000
```

Allow the location permission for best results. The Overpass and reverse-geocode
calls run from your browser, so live places need internet; otherwise the bundled
sample data is used.

## How suggestions are scored

Each candidate is scored on:

- alignment with your **veg/non-veg** slider and **order/dine-in** choice,
- **ranked cuisine** match (your 1st preference weighs most),
- **ambience** overlap (dine-in only),
- **occasion → budget** fit, plus **comfort + craving** boosts for period mode,
- **distance** (steeper penalty and a hard nearby cap for ordering), and
- your **learned preferences** from past ratings.

The single best match leads; two alternates follow.

## Files

| File | Purpose |
| --- | --- |
| `index.html` | Markup and the wizard steps |
| `styles.css` | Styling (neutral palette, slider, mode buttons, results) |
| `app.js` | Wizard flow, geolocation, Overpass lookup, scoring, learning |

## Privacy

Your location is used only in the browser to compute distances and query nearby
places; it's never sent to any server of ours. Ratings and learned preferences
live in `localStorage` and can be cleared any time via **Reset saved
preferences**.
