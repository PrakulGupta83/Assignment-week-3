/* =========================================================================
   TableTuner — smart restaurant recommender
   Helps someone who can't decide WHERE to order from / eat.

   Flow: current location -> veg/non-veg (slider) -> order/dine-in (buttons)
         -> cuisines (ranked by preference) -> ambience (dine-in only)
         -> occasion (incl. period comfort + cravings) -> one confident pick.

   Nearby places come from the free OpenStreetMap Overpass API using the
   device's precise location, with a curated demo fallback when location or
   network is unavailable. For ordering (and for period comfort) results are
   kept tightly local so you're not sent across the city. Ratings are saved to
   localStorage and nudge future suggestions.
   ========================================================================= */

/* ---------- App state ---------- */
const state = {
  location: null,          // { lat, lon, label }
  veg: 50,                 // 0 = veg, 100 = non-veg
  mode: 'order',           // 'order' (take-away) | 'dinein'
  cuisinesRanked: [],      // ordered by preference: [0] = 1st choice
  ambience: new Set(),
  occasion: null,
  cravings: new Set(),     // only meaningful when occasion === 'period'
};

// Wizard order. Ambience is skipped unless the user chose dine-in.
const ALL_STEPS = ['location', 'veg', 'mode', 'cuisine', 'ambience', 'occasion', 'results'];
let stepFlow = [...ALL_STEPS];
let stepPos = 0;

const LS_KEY = 'tabletuner_prefs_v1';

/* ---------- Learning store (localStorage) ---------- */
function loadPrefs() {
  try {
    return JSON.parse(localStorage.getItem(LS_KEY)) || { cuisine: {}, ambience: {}, ratings: {}, count: 0 };
  } catch {
    return { cuisine: {}, ambience: {}, ratings: {}, count: 0 };
  }
}
function savePrefs(p) {
  try { localStorage.setItem(LS_KEY, JSON.stringify(p)); } catch {}
}
let prefs = loadPrefs();

function renderLearnedNote() {
  const note = document.getElementById('learnedNote');
  const rated = Object.keys(prefs.ratings).length;
  if (rated === 0) {
    note.textContent = 'No ratings saved yet — rate places after ordering to personalize future picks.';
  } else {
    const top = topLearnedCuisine();
    note.textContent = `Learning from ${rated} rating${rated > 1 ? 's' : ''}` +
      (top ? ` · you seem to enjoy ${capitalize(top)}.` : '.');
  }
}
function topLearnedCuisine() {
  let best = null, bestScore = 0;
  for (const [k, v] of Object.entries(prefs.cuisine)) {
    if (v > bestScore) { bestScore = v; best = k; }
  }
  return bestScore > 0 ? best : null;
}

/* ---------- Step navigation ---------- */
function computeFlow() {
  // Ambience only matters for dine-in.
  stepFlow = ALL_STEPS.filter(s => !(s === 'ambience' && state.mode !== 'dinein'));
}

function showStep(name) {
  document.querySelectorAll('.step').forEach(el => {
    el.classList.toggle('step-active', el.dataset.step === name);
  });
  const questionSteps = stepFlow.filter(s => s !== 'location' && s !== 'results');
  const idx = questionSteps.indexOf(name);
  const pct = name === 'results' ? 100
    : name === 'location' ? 6
    : ((idx + 1) / (questionSteps.length + 1)) * 100;
  document.getElementById('progressBar').style.width = pct + '%';
}

function goNext() {
  if (stepFlow[stepPos] === 'mode') computeFlow(); // recompute after mode chosen
  if (stepPos < stepFlow.length - 1) {
    stepPos++;
    showStep(stepFlow[stepPos]);
  }
}
function goPrev() {
  if (stepPos > 0) {
    stepPos--;
    showStep(stepFlow[stepPos]);
  }
}

/* ---------- Geolocation (device location, high accuracy) ---------- */
function initLocation() {
  const statusEl = document.getElementById('locationStatus');
  const manualEl = document.getElementById('manualLocation');
  const nextBtn = document.getElementById('locationNext');

  function setLocation(lat, lon, label) {
    state.location = { lat, lon, label };
    document.getElementById('locationLabel').textContent = label;
    document.getElementById('locationChip').classList.add('ready');
    nextBtn.disabled = false;
  }

  function fallbackToManual() {
    statusEl.classList.add('hidden');
    manualEl.classList.remove('hidden');
    const sel = document.getElementById('cityPreset');
    const apply = () => {
      const [lat, lon] = sel.value.split(',').map(Number);
      setLocation(lat, lon, sel.options[sel.selectedIndex].text + ' (chosen)');
    };
    sel.addEventListener('change', apply);
    apply();
  }

  if (!navigator.geolocation) { fallbackToManual(); return; }
  navigator.geolocation.getCurrentPosition(
    async (pos) => {
      const { latitude: lat, longitude: lon } = pos.coords;
      statusEl.innerHTML = '<div class="spinner"></div><span>Got it! Naming your spot…</span>';
      const label = await reverseGeocode(lat, lon);
      statusEl.innerHTML = `<span>📍 Using your exact location — <strong>${label}</strong>. We'll only look right around you.</span>`;
      setLocation(lat, lon, label);
    },
    () => fallbackToManual(),
    { enableHighAccuracy: true, timeout: 8000, maximumAge: 30000 }
  );
}

async function reverseGeocode(lat, lon) {
  try {
    const url = `https://nominatim.openstreetmap.org/reverse?format=json&lat=${lat}&lon=${lon}&zoom=16`;
    const res = await fetch(url, { headers: { 'Accept': 'application/json' } });
    const data = await res.json();
    const a = data.address || {};
    return a.neighbourhood || a.suburb || a.road || a.city_district || a.town || a.city || a.county || 'your spot';
  } catch {
    return `${lat.toFixed(3)}, ${lon.toFixed(3)}`;
  }
}

/* ---------- Control labels ---------- */
function vegLabel(v) {
  if (v <= 20) return '🥗 Strictly veg';
  if (v <= 40) return 'Mostly veg';
  if (v < 60) return 'Either is fine';
  if (v < 80) return 'Mostly non-veg';
  return '🍗 Non-veg it is';
}

// Per-mode UI wording (order vs dine-in).
const MODE = {
  order:  { title: "Here's where we'd order from", time: 'Ready in', cta: 'Find it on Maps', badge: 'Order in', word: 'take-away' },
  dinein: { title: "Here's the table we'd book",   time: 'Reach in', cta: 'Get directions',  badge: 'Dine-in',  word: 'dine-in' },
};

/* ---------- Cuisine metadata ---------- */
const CUISINE_LABELS = {
  indian: 'Indian', italian: 'Italian', mexican: 'Mexican', chinese: 'Chinese',
  thai: 'Thai', japanese: 'Japanese', american: 'American', continental: 'Continental',
  cafe: 'Café', dessert: 'Dessert', other: 'Multi-cuisine',
};
const CUISINE_BILL = {
  indian: 500, italian: 750, mexican: 650, chinese: 550, thai: 800,
  japanese: 1200, american: 600, continental: 900, cafe: 400, dessert: 300, other: 600,
};

// Craving shortcuts shown for the "period comfort" occasion.
const CRAVING_CUISINES = {
  sweet: ['dessert', 'cafe'],
  spicy: ['indian', 'mexican', 'thai'],
  warm: ['thai', 'chinese', 'indian'],
  cheesy: ['italian', 'american'],
  cold: ['dessert', 'cafe'],
  hot: ['cafe'],
};
const CRAVING_LABELS = {
  sweet: 'something sweet', spicy: 'something spicy', warm: 'warm & comforting',
  cheesy: 'cheesy comfort', cold: 'a cold treat', hot: 'a hot drink',
};

/* ---------- Locality rules ---------- */
// Ordering (and period comfort) stay tight around you; dine-in allows travel.
function localeParams() {
  const tight = state.mode === 'order' || state.occasion === 'period';
  if (tight) return { radius: 2500, maxKm: 4, penalty: 11 };
  return { radius: 4500, maxKm: 18, penalty: 3.5 };
}

/* ---------- Fetch nearby restaurants ---------- */
async function fetchNearby(lat, lon, radius) {
  const query = `
    [out:json][timeout:25];
    (
      node["amenity"~"restaurant|cafe|fast_food"](around:${radius},${lat},${lon});
      way["amenity"~"restaurant|cafe|fast_food"](around:${radius},${lat},${lon});
    );
    out center 90;`;
  const res = await fetch('https://overpass-api.de/api/interpreter', {
    method: 'POST',
    body: 'data=' + encodeURIComponent(query),
  });
  if (!res.ok) throw new Error('overpass ' + res.status);
  const data = await res.json();
  const places = (data.elements || []).map(normalizeOSM).filter(Boolean);
  if (places.length === 0) throw new Error('empty');
  return places;
}

function normalizeOSM(el) {
  const t = el.tags || {};
  if (!t.name) return null;
  const lat = el.lat ?? el.center?.lat;
  const lon = el.lon ?? el.center?.lon;
  if (lat == null || lon == null) return null;

  const cuisine = mapCuisine(t.cuisine, t.amenity);
  let vegScore = 55;
  if (t['diet:vegetarian'] === 'only' || t['diet:vegan'] === 'only') vegScore = 5;
  else if (t['diet:vegetarian'] === 'yes') vegScore = 35;
  else if (t.cuisine && /kebab|steak|seafood|chicken|bbq|burger/i.test(t.cuisine)) vegScore = 80;

  const dineIn = t.takeaway === 'only' || t.amenity === 'fast_food' ? 25 : 70;

  return {
    id: 'osm' + el.type + el.id,
    name: t.name,
    cuisine,
    cuisineLabel: CUISINE_LABELS[cuisine] || 'Multi-cuisine',
    lat, lon,
    vegScore,
    dineInScore: dineIn,
    bill: estimateBill(t, cuisine),
    ambience: inferAmbience(t),
    source: 'osm',
  };
}

function mapCuisine(raw, amenity) {
  if (amenity === 'cafe') return 'cafe';
  if (!raw) return 'other';
  const s = raw.toLowerCase();
  if (/indian|punjabi|mughlai|south_indian|biryani/.test(s)) return 'indian';
  if (/italian|pizza|pasta/.test(s)) return 'italian';
  if (/mexican|tex-mex|burrito/.test(s)) return 'mexican';
  if (/chinese|szechuan|cantonese/.test(s)) return 'chinese';
  if (/thai/.test(s)) return 'thai';
  if (/japanese|sushi|ramen/.test(s)) return 'japanese';
  if (/american|burger|steak|bbq/.test(s)) return 'american';
  if (/coffee|cafe|tea/.test(s)) return 'cafe';
  if (/ice_cream|dessert|cake|bakery/.test(s)) return 'dessert';
  if (/continental|european|french/.test(s)) return 'continental';
  return 'other';
}

function estimateBill(t, cuisine) {
  const base = CUISINE_BILL[cuisine] || 600;
  const wobble = (hash(t.name || '') % 5 - 2) * 60;
  return Math.max(150, Math.round((base + wobble) / 50) * 50);
}

function inferAmbience(t) {
  const set = new Set();
  if (t.outdoor_seating === 'yes') set.add('outdoor');
  if (t.amenity === 'cafe') set.add('quiet');
  if (t.amenity === 'fast_food') set.add('casual');
  if (/bar|pub|brewery/i.test(t.cuisine || '') || t.amenity === 'bar') set.add('lively');
  return set;
}

function hash(s) {
  let h = 0;
  for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0;
  return Math.abs(h);
}

/* ---------- Demo fallback dataset (kept close to the user) ---------- */
function demoData(lat, lon) {
  // Small deltas so distances stay under ~1.5 km — reads as "around you".
  const seed = [
    ['Spice Route', 'indian', 0.004, 0.003, 15, 75, 550, ['family', 'casual']],
    ['Curry Leaf Kitchen', 'indian', -0.003, 0.005, 8, 30, 350, ['casual']],
    ['Bella Napoli', 'italian', 0.006, -0.002, 25, 80, 800, ['romantic', 'finedining']],
    ['Pasta & Co.', 'italian', -0.004, -0.004, 35, 55, 650, ['casual', 'family']],
    ['El Toro Cantina', 'mexican', 0.002, 0.006, 45, 70, 700, ['lively', 'friends']],
    ['Taco Fiesta', 'mexican', -0.005, 0.002, 20, 25, 400, ['casual']],
    ['Golden Dragon', 'chinese', 0.007, 0.004, 60, 65, 600, ['family']],
    ['Bangkok Bowl', 'thai', -0.002, -0.006, 15, 45, 750, ['quiet', 'romantic']],
    ['Sakura Sushi', 'japanese', 0.005, 0.005, 90, 85, 1300, ['finedining', 'quiet']],
    ['The Grill House', 'american', -0.006, 0.003, 30, 75, 700, ['lively', 'friends']],
    ['Corner Café', 'cafe', 0.001, -0.001, 30, 40, 350, ['quiet', 'outdoor']],
    ['Sweet Symphony', 'dessert', 0.003, 0.002, 20, 35, 300, ['casual', 'family']],
    ['Gelato Bar', 'dessert', -0.002, 0.003, 25, 40, 280, ['casual']],
    ['The Terrace', 'continental', 0.005, -0.005, 55, 88, 1100, ['outdoor', 'romantic', 'finedining']],
    ['Rooftop Social', 'continental', -0.004, 0.006, 40, 80, 950, ['outdoor', 'lively']],
  ];
  return seed.map((r, i) => {
    const [name, cuisine, dLat, dLon, veg, dineIn, bill, amb] = r;
    return {
      id: 'demo' + i,
      name, cuisine,
      cuisineLabel: CUISINE_LABELS[cuisine],
      lat: lat + dLat, lon: lon + dLon,
      vegScore: veg, dineInScore: dineIn, bill,
      ambience: new Set(amb),
      source: 'demo',
    };
  });
}

/* ---------- Geometry ---------- */
function haversineKm(lat1, lon1, lat2, lon2) {
  const R = 6371;
  const dLat = (lat2 - lat1) * Math.PI / 180;
  const dLon = (lon2 - lon1) * Math.PI / 180;
  const a = Math.sin(dLat / 2) ** 2 +
    Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) * Math.sin(dLon / 2) ** 2;
  return R * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
}
function travelMinutes(km, isTakeaway) {
  // ~22 km/h effective city speed; ordering adds ~12 min prep/handoff.
  const drive = (km / 22) * 60;
  return Math.max(4, Math.round(drive + (isTakeaway ? 12 : 0)));
}

/* ---------- Scoring ---------- */
function scorePlace(p, dist, penalty) {
  let score = 100;

  // Veg preference alignment.
  score -= Math.abs(state.veg - p.vegScore) * 0.35;

  // Mode alignment: for ordering, dine-in-only places are less ideal; for
  // dine-in, take-away-only places are less ideal.
  if (state.mode === 'order') score -= p.dineInScore * 0.18;
  else score -= (100 - p.dineInScore) * 0.15;

  // Ranked cuisine: earlier preference => bigger boost; unlisted => penalty.
  if (state.cuisinesRanked.length) {
    const r = state.cuisinesRanked.indexOf(p.cuisine);
    if (r === 0) score += 34;
    else if (r === 1) score += 22;
    else if (r === 2) score += 14;
    else if (r > 2) score += 8;
    else score -= 30;
  }

  // Ambience match (dine-in only).
  if (state.mode === 'dinein' && state.ambience.size) {
    const overlap = [...state.ambience].filter(a => p.ambience.has(a)).length;
    score += overlap * 12;
  }

  // Occasion → budget fit.
  score -= Math.abs(p.bill - occasionBudget(state.occasion)) / 40;

  // Period comfort: nudge toward comforting cuisines and any craving picked.
  if (state.occasion === 'period') {
    const comfort = { dessert: 20, cafe: 14, indian: 14, italian: 13, chinese: 11, thai: 8 };
    score += comfort[p.cuisine] || 0;
    for (const c of state.cravings) {
      if ((CRAVING_CUISINES[c] || []).includes(p.cuisine)) score += 12;
    }
  }

  // Distance penalty (steeper for ordering / period so it stays local).
  score -= dist * penalty;

  // Learned preferences.
  score += (prefs.cuisine[p.cuisine] || 0) * 6;
  if (state.mode === 'dinein') {
    for (const a of p.ambience) score += (prefs.ambience[a] || 0) * 3;
  }
  if (prefs.ratings[p.name]) score += (prefs.ratings[p.name] - 3) * 8;

  return score;
}

function occasionBudget(occ) {
  return {
    everyday: 400, quick: 300, friends: 600, family: 700,
    date: 900, business: 1000, celebration: 1200, period: 450,
  }[occ] || 600;
}

function occasionText(o) {
  return {
    everyday: 'an everyday meal', date: 'a date night', family: 'a family gathering',
    friends: 'friends', business: 'a business meal', celebration: 'a celebration',
    quick: 'a quick bite', period: 'some comfort right now',
  }[o] || o;
}

/* ---------- Results ---------- */
async function runRecommendation() {
  stepPos = stepFlow.indexOf('results');
  showStep('results');

  const loading = document.getElementById('loadingResults');
  const noResults = document.getElementById('noResults');
  loading.classList.remove('hidden');
  noResults.classList.add('hidden');
  const allWrap = document.getElementById('allWrap');
  const toggleAll = document.getElementById('toggleAll');
  allWrap.classList.add('hidden');
  toggleAll.classList.remove('hidden');
  toggleAll.textContent = 'See all nearby options ▾';

  const { lat, lon } = state.location;
  const lp = localeParams();
  let places, usedDemo = false;
  try {
    document.getElementById('loadingText').textContent = 'Looking right around you…';
    places = await fetchNearby(lat, lon, lp.radius);
  } catch (e) {
    usedDemo = true;
    places = demoData(lat, lon);
  }

  const isTakeaway = state.mode === 'order';
  let ranked = places.map(p => {
    const dist = haversineKm(lat, lon, p.lat, p.lon);
    const mins = travelMinutes(dist, isTakeaway);
    const score = scorePlace(p, dist, lp.penalty);
    return { ...p, dist, mins, score };
  }).sort((a, b) => b.score - a.score);

  // Enforce locality — but never show an empty screen.
  const near = ranked.filter(p => p.dist <= lp.maxKm);
  if (near.length >= 1) ranked = near;

  const max = ranked.length ? ranked[0].score : 1;
  const min = ranked.length ? ranked[ranked.length - 1].score : 0;
  const range = Math.max(1, max - min);

  const top = ranked.slice(0, 8);
  loading.classList.add('hidden');

  if (top.length === 0) {
    document.getElementById('pickArea').classList.add('hidden');
    noResults.classList.remove('hidden');
    return;
  }
  document.getElementById('pickArea').classList.remove('hidden');

  renderPick(ranked, min, range);
  renderTable(top, max, range, min);

  document.getElementById('resultsTitle').textContent = MODE[state.mode].title;

  const bits = [vegLabel(state.veg), MODE[state.mode].word];
  if (state.cuisinesRanked.length) bits.push(state.cuisinesRanked.map(c => CUISINE_LABELS[c]).join(' → '));
  if (state.occasion) bits.push('for ' + occasionText(state.occasion));
  document.getElementById('resultsSummary').textContent =
    `Based on your mood · ${bits.join(' · ')} · near ${state.location.label}` +
    (usedDemo ? '  (showing sample data — live lookup unavailable)' : '');
}

/* Convert a raw score into a friendly 55-100 match percentage. */
function matchPct(score, min, range) {
  return Math.round(((score - min) / range) * 45 + 55);
}

const AMB_WORD = {
  romantic: 'romantic', family: 'family-friendly', casual: 'laid-back',
  lively: 'lively', finedining: 'fine-dining', outdoor: 'open-air', quiet: 'cozy',
};

/* Plain-language "why this one" so an undecided user can trust the pick. */
function buildReason(p) {
  const parts = [];
  const r = state.cuisinesRanked.indexOf(p.cuisine);
  if (r === 0) parts.push(`it's your top pick, <strong>${p.cuisineLabel}</strong>`);
  else if (r > 0) parts.push(`it serves the <strong>${p.cuisineLabel}</strong> you listed`);
  else parts.push(`the <strong>${p.cuisineLabel}</strong> here fits your mood`);

  if (state.veg <= 40 && p.vegScore <= 45) parts.push('it leans veg, like you wanted');
  else if (state.veg >= 60 && p.vegScore >= 55) parts.push("there's plenty non-veg on the menu");

  if (state.mode === 'dinein' && state.ambience.size) {
    const overlap = [...state.ambience].filter(a => p.ambience.has(a));
    if (overlap.length) parts.push(`it has the ${AMB_WORD[overlap[0]] || overlap[0]} vibe you picked`);
  }

  if (state.occasion === 'period') {
    const crav = [...state.cravings].map(c => CRAVING_LABELS[c]);
    parts.push(crav.length ? `it's comfort food for ${crav.join(' & ')}` : "it's easy comfort food");
  } else if (state.occasion) {
    parts.push(`it's right for ${occasionText(state.occasion)}`);
  }

  parts.push(state.mode === 'order'
    ? `and it's close by (<strong>${p.dist.toFixed(1)} km</strong>), so it'll arrive hot`
    : `and it's an easy <strong>${p.dist.toFixed(1)} km</strong> away`);

  return 'Because ' + parts.join(', ').replace(/, and /, ' — and ') + '.';
}

/* Render the hero pick and up to two compact alternates. */
function renderPick(ranked, min, range) {
  const hero = ranked[0];
  const heroEl = document.getElementById('topPick');
  const match = matchPct(hero.score, min, range);
  const badgeClass = state.mode === 'order' ? 'order' : 'dinein';

  heroEl.innerHTML = `
    <div class="pick-badge">✨ Our pick for you</div>
    <div class="pick-title">${escapeHtml(hero.name)}</div>
    <div class="pick-tags">
      <span class="badge">${hero.cuisineLabel}</span>
      <span class="badge ${badgeClass}">${MODE[state.mode].badge}</span>
      <span class="badge">${match}% match</span>
    </div>
    <p class="pick-reason">${buildReason(hero)}</p>
    <div class="pick-stats">
      <div class="pick-stat"><span class="stat-label">Distance</span><span class="stat-val">${hero.dist.toFixed(1)} km</span></div>
      <div class="pick-stat"><span class="stat-label">${MODE[state.mode].time}</span><span class="stat-val">~${hero.mins} min</span></div>
      <div class="pick-stat"><span class="stat-label">Avg bill</span><span class="stat-val">₹${hero.bill}<span style="font-size:12px;font-weight:400;color:var(--muted)">/person</span></span></div>
    </div>
    <div class="pick-actions">
      <a class="btn btn-primary" href="${mapsLink(hero)}" target="_blank" rel="noopener">${MODE[state.mode].cta} ↗</a>
      <div class="pick-rate">
        <span class="rate-label">${state.mode === 'order' ? 'Ordered' : 'Been'} here? Rate it:</span>
        <div class="stars" data-name="${escapeHtml(hero.name)}" data-cuisine="${hero.cuisine}"></div>
      </div>
    </div>`;
  renderStars(heroEl.querySelector('.stars'), prefs.ratings[hero.name] || 0);

  const alts = ranked.slice(1, 3);
  const wrap = document.getElementById('alternatesWrap');
  const altBox = document.getElementById('alternates');
  altBox.innerHTML = '';
  if (alts.length === 0) { wrap.classList.add('hidden'); return; }
  wrap.classList.remove('hidden');
  alts.forEach(p => {
    const m = matchPct(p.score, min, range);
    const card = document.createElement('div');
    card.className = 'alt-card';
    card.innerHTML = `
      <div class="alt-head">
        <span class="alt-name">${escapeHtml(p.name)}</span>
        <span class="badge">${m}%</span>
      </div>
      <div class="alt-sub">${p.cuisineLabel} · ${p.dist.toFixed(1)} km · ~${p.mins} min · ₹${p.bill}</div>
      <div class="alt-foot">
        <a class="map-link" href="${mapsLink(p)}" target="_blank" rel="noopener">Open in Maps ↗</a>
        <div class="stars" data-name="${escapeHtml(p.name)}" data-cuisine="${p.cuisine}"></div>
      </div>`;
    altBox.appendChild(card);
    renderStars(card.querySelector('.stars'), prefs.ratings[p.name] || 0);
  });
}

function mapsLink(p) {
  return `https://www.google.com/maps/search/?api=1&query=${encodeURIComponent(p.name)}%20${p.lat},${p.lon}`;
}

function renderTable(list, max, range, min) {
  const body = document.getElementById('resultsBody');
  body.innerHTML = '';
  const badgeClass = state.mode === 'order' ? 'order' : 'dinein';
  list.forEach((p, i) => {
    const match = matchPct(p.score, min, range);
    const savedRating = prefs.ratings[p.name] || 0;
    const tr = document.createElement('tr');
    tr.innerHTML = `
      <td>${i + 1}</td>
      <td>
        <div class="rest-name">${escapeHtml(p.name)}</div>
        <div class="rest-sub">${p.source === 'demo' ? 'sample' : 'nearby'}</div>
      </td>
      <td><span class="badge">${p.cuisineLabel}</span></td>
      <td><span class="badge ${badgeClass}">${MODE[state.mode].badge}</span></td>
      <td>${p.dist.toFixed(1)} km</td>
      <td>${p.mins} min</td>
      <td>₹${p.bill}/person</td>
      <td>
        <div class="match-bar">
          <div class="match-track"><div class="match-fill" style="width:${match}%"></div></div>
          <span>${match}%</span>
        </div>
      </td>
      <td><a class="map-link" href="${mapsLink(p)}" target="_blank" rel="noopener">Open ↗</a></td>
      <td><div class="stars" data-name="${escapeHtml(p.name)}" data-cuisine="${p.cuisine}"></div></td>
    `;
    body.appendChild(tr);
    renderStars(tr.querySelector('.stars'), savedRating);
  });
}

function renderStars(container, current) {
  container.innerHTML = '';
  for (let s = 1; s <= 5; s++) {
    const star = document.createElement('span');
    star.className = 'star' + (s <= current ? ' on' : '');
    star.textContent = '★';
    star.addEventListener('click', () => {
      recordRating(container.dataset.name, container.dataset.cuisine, s);
      renderStars(container, s);
    });
    container.appendChild(star);
  }
}

/* ---------- Learning from ratings ---------- */
function recordRating(name, cuisine, stars) {
  const prev = prefs.ratings[name] || 0;
  prefs.ratings[name] = stars;

  const delta = (stars - 3);
  const prevDelta = prev ? (prev - 3) : 0;
  const net = delta - prevDelta;

  prefs.cuisine[cuisine] = clamp((prefs.cuisine[cuisine] || 0) + net * 0.5, -5, 5);
  for (const a of state.ambience) {
    prefs.ambience[a] = clamp((prefs.ambience[a] || 0) + net * 0.3, -5, 5);
  }

  prefs.count = (prefs.count || 0) + (prev ? 0 : 1);
  savePrefs(prefs);
  renderLearnedNote();
  toast(stars >= 4 ? `Noted — we'll show you more like ${CUISINE_LABELS[cuisine] || cuisine}.`
    : stars <= 2 ? `Got it — we'll ease off on that.` : 'Thanks for the rating!');
}

function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

/* ---------- Tiny toast ---------- */
let toastTimer;
function toast(msg) {
  let el = document.getElementById('toast');
  if (!el) {
    el = document.createElement('div');
    el.id = 'toast';
    el.style.cssText =
      'position:fixed;left:50%;bottom:28px;transform:translateX(-50%);' +
      'background:#262a45;border:1px solid #2f3358;color:#eef0ff;padding:12px 20px;' +
      'border-radius:12px;box-shadow:0 12px 30px rgba(0,0,0,.4);z-index:99;' +
      'font-size:14px;transition:opacity .3s;opacity:0;';
    document.body.appendChild(el);
  }
  el.textContent = msg;
  el.style.opacity = '1';
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => { el.style.opacity = '0'; }, 2400);
}

/* ---------- Utils ---------- */
function escapeHtml(s) {
  return String(s).replace(/[&<>"']/g, c =>
    ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
}
function capitalize(s) { return s ? s[0].toUpperCase() + s.slice(1) : s; }

/* ---------- Wiring ---------- */
function wireMultiChips(containerId, targetSet) {
  const container = document.getElementById(containerId);
  container.addEventListener('click', (e) => {
    const chip = e.target.closest('.chip');
    if (!chip) return;
    chip.classList.toggle('selected');
    const val = chip.dataset.value;
    if (targetSet.has(val)) targetSet.delete(val); else targetSet.add(val);
  });
}

// Cuisines: each click appends to the ranked list; clicking again removes and
// renumbers. The badge on each selected chip shows its preference order.
function wireCuisineRanking() {
  const container = document.getElementById('cuisineChips');
  container.addEventListener('click', (e) => {
    const chip = e.target.closest('.chip');
    if (!chip) return;
    const val = chip.dataset.value;
    const i = state.cuisinesRanked.indexOf(val);
    if (i === -1) state.cuisinesRanked.push(val);
    else state.cuisinesRanked.splice(i, 1);
    renderCuisineRanks();
  });
}
function renderCuisineRanks() {
  document.querySelectorAll('#cuisineChips .chip').forEach(chip => {
    const r = state.cuisinesRanked.indexOf(chip.dataset.value);
    chip.classList.toggle('selected', r !== -1);
    let badge = chip.querySelector('.rank');
    if (r === -1) {
      if (badge) badge.remove();
    } else {
      if (!badge) { badge = document.createElement('span'); badge.className = 'rank'; chip.prepend(badge); }
      badge.textContent = String(r + 1);
    }
  });
}

function wireOccasion() {
  const container = document.getElementById('occasionChips');
  const cravingBlock = document.getElementById('cravingBlock');
  container.addEventListener('click', (e) => {
    const chip = e.target.closest('.chip');
    if (!chip) return;
    container.querySelectorAll('.chip').forEach(c => c.classList.remove('selected'));
    chip.classList.add('selected');
    state.occasion = chip.dataset.value;
    if (state.occasion === 'period') {
      cravingBlock.classList.remove('hidden');
    } else {
      cravingBlock.classList.add('hidden');
      state.cravings.clear();
      document.querySelectorAll('#cravingChips .chip').forEach(c => c.classList.remove('selected'));
    }
  });
}

function init() {
  initLocation();
  renderLearnedNote();

  // Veg slider
  const vegSlider = document.getElementById('vegSlider');
  const vegValue = document.getElementById('vegValue');
  vegSlider.addEventListener('input', () => {
    state.veg = +vegSlider.value;
    vegValue.textContent = vegLabel(state.veg);
  });

  // Mode segmented buttons
  const modeSeg = document.getElementById('modeSegmented');
  modeSeg.addEventListener('click', (e) => {
    const btn = e.target.closest('.seg-btn');
    if (!btn) return;
    modeSeg.querySelectorAll('.seg-btn').forEach(b => b.classList.remove('is-active'));
    btn.classList.add('is-active');
    state.mode = btn.dataset.value;
    computeFlow();
  });

  // Chips
  wireCuisineRanking();
  wireMultiChips('ambienceChips', state.ambience);
  wireMultiChips('cravingChips', state.cravings);
  wireOccasion();

  // Nav
  document.querySelectorAll('[data-next]').forEach(b => b.addEventListener('click', goNext));
  document.querySelectorAll('[data-prev]').forEach(b => b.addEventListener('click', goPrev));

  document.getElementById('findBtn').addEventListener('click', () => {
    if (!state.occasion) { toast('Pick an occasion to continue.'); return; }
    runRecommendation();
  });

  document.getElementById('restartBtn').addEventListener('click', () => {
    stepPos = 0;
    computeFlow();
    showStep('location');
  });

  document.getElementById('toggleAll').addEventListener('click', (e) => {
    const wrap = document.getElementById('allWrap');
    const open = wrap.classList.toggle('hidden') === false;
    e.target.textContent = open ? 'Hide the full list ▴' : 'See all nearby options ▾';
  });

  document.getElementById('resetLearning').addEventListener('click', () => {
    if (confirm('Clear all saved ratings and learned preferences?')) {
      prefs = { cuisine: {}, ambience: {}, ratings: {}, count: 0 };
      savePrefs(prefs);
      renderLearnedNote();
      toast('Saved preferences cleared.');
    }
  });

  computeFlow();
  showStep('location');
}

document.addEventListener('DOMContentLoaded', init);
