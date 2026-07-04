# TableTuner — Smart Restaurant Recommender

A single-page web app that asks a few quick questions and then recommends
restaurants near you — for dine-in or take-away — ranked by distance, travel
time, average bill and how well they match your mood. Rate places after you
visit and the app quietly tunes future suggestions toward what you like.

> This directory also contains an earlier R exercise (`InverseMatrix.R`) from
> the original Week-3 assignment; it is unrelated to this app.

## Features

- **Guided wizard**, asking questions in this exact order:
  1. **Veg / Non-veg** — a left↔right slider (middle = "either is fine")
  2. **Order / Dine-in** — a left↔right slider
  3. **Food type preference** — Indian, Italian, Mexican, Chinese, Thai,
     Japanese, American, Continental, Café, Dessert (multi-select)
  4. **Ambience preference** — shown *only when you lean toward dine-in*
     (romantic, family, casual, lively, fine dining, outdoor, quiet)
  5. **Occasion** — everyday, date night, family, friends, business,
     celebration, quick bite
- **Uses your current location** via the browser Geolocation API to measure
  real distance and travel time. If you decline or it's unavailable, you can
  pick a starting city instead.
- **Real nearby restaurants** pulled live from the free OpenStreetMap
  [Overpass API](https://overpass-api.de/) — no API key required. If the live
  lookup can't be reached, it falls back to a built-in sample dataset so the
  app always works.
- **Results table** with distance, estimated travel time, estimated average
  bill per person, a match score, and a **Google Maps link** for each place.
- **Learns from your ratings** — the 1–5 star rating you give each place is
  saved in your browser (`localStorage`) and adjusts future rankings toward
  the cuisines and ambiences you rate highly.

## Running it

It's a static site — no build step or server required.

```bash
# Option A: just open the file
open index.html          # macOS  (use `xdg-open` on Linux)

# Option B: serve it (needed on some browsers for geolocation over http)
python3 -m http.server 8000
# then visit http://localhost:8000
```

Allow the location permission when prompted for the best results. The Overpass
and reverse-geocoding calls run from your browser, so the app needs internet
access to show live places; otherwise it uses the bundled sample data.

## How suggestions are scored

Each candidate is scored on:

- alignment with your **veg/non-veg** and **order/dine-in** slider positions,
- **cuisine** match against your selections,
- **ambience** overlap (dine-in only),
- **occasion → budget** fit (e.g. a celebration nudges toward higher bills),
- **distance** (closer is better), and
- your **learned preferences** from past ratings.

The top matches are shown, highest first.

## Files

| File | Purpose |
| --- | --- |
| `index.html` | Markup and the wizard steps |
| `styles.css` | Styling (dark theme, sliders, results table) |
| `app.js` | Wizard flow, geolocation, Overpass lookup, scoring, learning |

## Privacy

Your location is used only in the browser to compute distances and to query
nearby places; it is never sent to any server of ours. Ratings and learned
preferences live in your browser's `localStorage` and can be cleared any time
with the **Reset saved preferences** link.
