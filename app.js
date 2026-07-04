/* =========================================================================
   TableTuner — smart restaurant recommender
   - Wizard: location -> veg/non-veg -> order/dine-in -> cuisine -> ambience
             -> occasion -> results table
   - Real nearby restaurants via the free OpenStreetMap Overpass API,
     with a curated demo fallback when location/network is unavailable.
   - Learns from the star ratings you give (saved in localStorage) and
     nudges future suggestions toward what you liked.
   ========================================================================= */

/* ---------- App state ---------- */
const state = {
  location: null,          // { lat, lon, label }
  veg: 50,                 // 0 = veg, 100 = non-veg
  mode: 50,                // 0 = order/take-away, 100 = dine-in
  cuisines: new Set(),
  ambience: new Set(),
  occasion: null,
};

// Which steps make up the wizard, in order. "ambience" is skipped when the
// user leans toward take-away rather than dine-in.
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
  localStorage.setItem(LS_KEY, JSON.stringify(p));
}
let prefs = loadPrefs();

function renderLearnedNote() {
  const note = document.getElementById('learnedNote');
  const rated = Object.keys(prefs.ratings).length;
  if (rated === 0) {
    note.textContent = 'No ratings saved yet — rate places after visiting to personalize future picks.';
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
  // Skip ambience if user leans toward ordering in (mode < 40).
  stepFlow = ALL_STEPS.filter(s => !(s === 'ambience' && state.mode < 40));
}

function showStep(name) {
  document.querySelectorAll('.step').forEach(el => {
    el.classList.toggle('step-active', el.dataset.step === name);
  });
  // Progress = position among question steps (exclude location + results).
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

/* ---------- Geolocation ---------- */
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

  function fallbackToManual(reason) {
    statusEl.classList.add('hidden');
    manualEl.classList.remove('hidden');
    const sel = document.getElementById('cityPreset');
    const apply = () => {
      const [lat, lon] = sel.value.split(',').map(Number);
      setLocation(lat, lon, sel.options[sel.selectedIndex].text + ' (chosen)');
    };
    sel.addEventListener('change', apply);
    apply(); // default to first city so the user can continue
  }

  if (!navigator.geolocation) {
    fallbackToManual('unsupported');
    return;
  }
  navigator.geolocation.getCurrentPosition(
    async (pos) => {
      const { latitude: lat, longitude: lon } = pos.coords;
      statusEl.innerHTML = '<div class="spinner"></div><span>Got it! Naming your area…</span>';
      const label = await reverseGeocode(lat, lon);
      statusEl.innerHTML = `<span>📍 Using your current location — <strong>${label}</strong></span>`;
      setLocation(lat, lon, label);
    },
    () => fallbackToManual('denied'),
    { enableHighAccuracy: true, timeout: 8000, maximumAge: 60000 }
  );
}

async function reverseGeocode(lat, lon) {
  try {
    const url = `https://nominatim.openstreetmap.org/reverse?format=json&lat=${lat}&lon=${lon}&zoom=14`;
    const res = await fetch(url, { headers: { 'Accept': 'application/json' } });
    const data = await res.json();
    const a = data.address || {};
    return a.suburb || a.neighbourhood || a.city_district || a.town || a.city || a.county || 'your area';
  } catch {
    return `${lat.toFixed(3)}, ${lon.toFixed(3)}`;
  }
}

/* ---------- Slider labels ---------- */
function vegLabel(v) {
  if (v <= 20) return '🥗 Strictly veg';
  if (v <= 40) return 'Mostly veg';
  if (v < 60) return 'Either is fine';
  if (v < 80) return 'Mostly non-veg';
  return '🍗 Non-veg it is';
}
function modeLabel(v) {
  if (v <= 20) return '🛵 Take-away / order in';
  if (v <= 40) return 'Leaning take-away';
  if (v < 60) return 'No preference';
  if (v < 80) return 'Leaning dine-in';
  return '🍷 Dine-in experience';
}

/* ---------- Cuisine metadata (for demo data + estimates) ---------- */
const CUISINE_LABELS = {
  indian: 'Indian', italian: 'Italian', mexican: 'Mexican', chinese: 'Chinese',
  thai: 'Thai', japanese: 'Japanese', american: 'American', continental: 'Continental',
  cafe: 'Café', dessert: 'Dessert', other: 'Multi-cuisine',
};
// Rough per-head bill band (INR) used only when OSM has no price info.
const CUISINE_BILL = {
  indian: 500, italian: 750, mexican: 650, chinese: 550, thai: 800,
  japanese: 1200, american: 600, continental: 900, cafe: 400, dessert: 300, other: 600,
};

/* ---------- Fetch nearby restaurants ---------- */
async function fetchNearby(lat, lon) {
  const query = `
    [out:json][timeout:25];
    (
      node["amenity"~"restaurant|cafe|fast_food"](around:3500,${lat},${lon});
      way["amenity"~"restaurant|cafe|fast_food"](around:3500,${lat},${lon});
    );
    out center 80;`;
  const res = await fetch('https://overpass-api.de/api/interpreter', {
    method: 'POST',
    body: 'data=' + encodeURIComponent(query),
  });
  if (!res.ok) throw new Error('overpass ' + res.status);
  const data = await res.json();
  const places = (data.elements || [])
    .map(el => normalizeOSM(el))
    .filter(Boolean);
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
  // veg score of the place: 0 veg -> 100 non-veg
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
  // OSM sometimes carries a price band via `price` or currency tags; otherwise
  // fall back to a cuisine heuristic with a small deterministic wobble.
  const base = CUISINE_BILL[cuisine] || 600;
  const wobble = (hash(t.name || '') % 5 - 2) * 60; // -120..+120, stable per name
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

/* ---------- Demo fallback dataset (relative to chosen location) ---------- */
function demoData(lat, lon) {
  const seed = [
    ['Spice Route', 'indian', 0.006, 0.004, 15, 75, 550, ['family', 'casual']],
    ['Curry Leaf Kitchen', 'indian', -0.004, 0.008, 8, 30, 350, ['casual']],
    ['Bella Napoli', 'italian', 0.010, -0.003, 25, 80, 800, ['romantic', 'finedining']],
    ['Pasta & Co.', 'italian', -0.007, -0.006, 35, 55, 650, ['casual', 'family']],
    ['El Toro Cantina', 'mexican', 0.003, 0.011, 45, 70, 700, ['lively', 'friends']],
    ['Taco Fiesta', 'mexican', -0.009, 0.002, 20, 25, 400, ['casual']],
    ['Golden Dragon', 'chinese', 0.012, 0.006, 60, 65, 600, ['family']],
    ['Bangkok Bowl', 'thai', -0.002, -0.010, 15, 45, 750, ['quiet', 'romantic']],
    ['Sakura Sushi', 'japanese', 0.008, 0.009, 90, 85, 1300, ['finedining', 'quiet']],
    ['The Grill House', 'american', -0.011, 0.004, 30, 75, 700, ['lively', 'friends']],
    ['Corner Café', 'cafe', 0.002, -0.002, 30, 40, 350, ['quiet', 'outdoor']],
    ['Sweet Symphony', 'dessert', 0.004, 0.003, 20, 35, 300, ['casual', 'family']],
    ['The Terrace', 'continental', 0.009, -0.008, 55, 88, 1100, ['outdoor', 'romantic', 'finedining']],
    ['Rooftop Social', 'continental', -0.006, 0.010, 40, 80, 950, ['outdoor', 'lively']],
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
function travelMinutes(km, dineIn) {
  // ~22 km/h effective city speed; take-away adds ~12 min prep/handoff.
  const drive = (km / 22) * 60;
  return Math.max(4, Math.round(drive + (dineIn ? 0 : 12)));
}

/* ---------- Scoring ---------- */
function scorePlace(p, dist) {
  let score = 100;

  // Veg preference alignment (0..100 distance between user & place).
  score -= Math.abs(state.veg - p.vegScore) * 0.35;

  // Mode alignment.
  score -= Math.abs(state.mode - p.dineInScore) * 0.25;

  // Cuisine match (explicit selection is a strong signal).
  if (state.cuisines.size > 0) {
    score += state.cuisines.has(p.cuisine) ? 30 : -35;
  }

  // Ambience match (only meaningful for dine-in).
  if (state.mode >= 40 && state.ambience.size > 0) {
    const overlap = [...state.ambience].filter(a => p.ambience.has(a)).length;
    score += overlap * 12;
  }

  // Occasion → budget fit.
  const target = occasionBudget(state.occasion);
  score -= Math.abs(p.bill - target) / 40;

  // Distance penalty.
  score -= dist * 4;

  // Learned preferences.
  score += (prefs.cuisine[p.cuisine] || 0) * 6;
  if (state.mode >= 40) {
    for (const a of p.ambience) score += (prefs.ambience[a] || 0) * 3;
  }
  // Places you personally rated well get a direct boost.
  if (prefs.ratings[p.name]) score += (prefs.ratings[p.name] - 3) * 8;

  return score;
}

function occasionBudget(occ) {
  return {
    everyday: 400, quick: 300, friends: 600, family: 700,
    date: 900, business: 1000, celebration: 1200,
  }[occ] || 600;
}

/* ---------- Results rendering ---------- */
async function runRecommendation() {
  stepPos = stepFlow.indexOf('results');
  showStep('results');

  const loading = document.getElementById('loadingResults');
  const table = document.getElementById('resultsTable');
  const noResults = document.getElementById('noResults');
  loading.classList.remove('hidden');
  table.classList.add('hidden');
  noResults.classList.add('hidden');

  const { lat, lon } = state.location;
  let places, usedDemo = false;
  try {
    document.getElementById('loadingText').textContent = 'Scanning restaurants near you…';
    places = await fetchNearby(lat, lon);
  } catch (e) {
    usedDemo = true;
    places = demoData(lat, lon);
  }

  // Score + sort.
  const wantDineIn = state.mode >= 40;
  const ranked = places.map(p => {
    const dist = haversineKm(lat, lon, p.lat, p.lon);
    const mins = travelMinutes(dist, wantDineIn ? 1 : 0);
    const score = scorePlace(p, dist);
    return { ...p, dist, mins, score };
  }).sort((a, b) => b.score - a.score);

  // Normalize a friendly 0-100 "match" for display.
  const max = ranked.length ? ranked[0].score : 1;
  const min = ranked.length ? ranked[ranked.length - 1].score : 0;
  const range = Math.max(1, max - min);

  const top = ranked.slice(0, 8);
  loading.classList.add('hidden');

  if (top.length === 0) {
    noResults.classList.remove('hidden');
    return;
  }

  renderTable(top, max, range, min, wantDineIn);
  table.classList.remove('hidden');

  const summary = document.getElementById('resultsSummary');
  const bits = [];
  bits.push(vegLabel(state.veg));
  bits.push(wantDineIn ? 'dine-in' : 'take-away');
  if (state.cuisines.size) bits.push([...state.cuisines].map(c => CUISINE_LABELS[c]).join(', '));
  if (state.occasion) bits.push('for ' + occasionText(state.occasion));
  summary.textContent = `Near ${state.location.label} · ${bits.join(' · ')}` +
    (usedDemo ? '  (showing sample data — live lookup unavailable)' : '');
}

function occasionText(o) {
  return {
    everyday: 'an everyday meal', date: 'a date night', family: 'a family gathering',
    friends: 'friends', business: 'a business meal', celebration: 'a celebration', quick: 'a quick bite',
  }[o] || o;
}

function renderTable(list, max, range, min, wantDineIn) {
  const body = document.getElementById('resultsBody');
  body.innerHTML = '';
  list.forEach((p, i) => {
    const match = Math.round(((p.score - min) / range) * 45 + 55); // map to 55-100
    const mapUrl = `https://www.google.com/maps/search/?api=1&query=${encodeURIComponent(p.name)}%20${p.lat},${p.lon}`;
    const modeBadge = wantDineIn
      ? '<span class="badge dinein">Dine-in</span>'
      : '<span class="badge order">Order</span>';
    const savedRating = prefs.ratings[p.name] || 0;

    const tr = document.createElement('tr');
    tr.innerHTML = `
      <td>${i + 1}</td>
      <td>
        <div class="rest-name">${escapeHtml(p.name)}</div>
        <div class="rest-sub">${p.source === 'demo' ? 'sample' : 'nearby'}</div>
      </td>
      <td><span class="badge">${p.cuisineLabel}</span></td>
      <td>${modeBadge}</td>
      <td>${p.dist.toFixed(1)} km</td>
      <td>${p.mins} min</td>
      <td>₹${p.bill}/person</td>
      <td>
        <div class="match-bar">
          <div class="match-track"><div class="match-fill" style="width:${match}%"></div></div>
          <span>${match}%</span>
        </div>
      </td>
      <td><a class="map-link" href="${mapUrl}" target="_blank" rel="noopener">Open ↗</a></td>
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

  // Adjust cuisine/ambience weights: high stars (4-5) push up, low (1-2) push down.
  const delta = (stars - 3); // -2..+2
  const prevDelta = prev ? (prev - 3) : 0;
  const net = delta - prevDelta;

  prefs.cuisine[cuisine] = clamp((prefs.cuisine[cuisine] || 0) + net * 0.5, -5, 5);

  // Ambience learning: attribute to currently selected ambience prefs.
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
      'background:#262845;border:1px solid #2e3155;color:#eef0ff;padding:12px 20px;' +
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
function wireChips(containerId, targetSet, single) {
  const container = document.getElementById(containerId);
  container.addEventListener('click', (e) => {
    const chip = e.target.closest('.chip');
    if (!chip) return;
    if (single) {
      container.querySelectorAll('.chip').forEach(c => c.classList.remove('selected'));
      chip.classList.add('selected');
      state.occasion = chip.dataset.value;
    } else {
      chip.classList.toggle('selected');
      const val = chip.dataset.value;
      if (targetSet.has(val)) targetSet.delete(val);
      else targetSet.add(val);
    }
  });
}

function init() {
  initLocation();
  renderLearnedNote();

  // Sliders
  const vegSlider = document.getElementById('vegSlider');
  const vegValue = document.getElementById('vegValue');
  vegSlider.addEventListener('input', () => {
    state.veg = +vegSlider.value;
    vegValue.textContent = vegLabel(state.veg);
  });
  const modeSlider = document.getElementById('modeSlider');
  const modeValue = document.getElementById('modeValue');
  modeSlider.addEventListener('input', () => {
    state.mode = +modeSlider.value;
    modeValue.textContent = modeLabel(state.mode);
  });

  // Chips
  wireChips('cuisineChips', state.cuisines, false);
  wireChips('ambienceChips', state.ambience, false);
  wireChips('occasionChips', null, true);

  // Nav buttons
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
