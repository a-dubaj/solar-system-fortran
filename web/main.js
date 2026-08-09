// main.js
// Renders the solar system using orbital positions computed by the
// Fortran engine (compiled to WebAssembly). All physics/geometry math
// lives in Fortran; this file only handles rendering and user input.

const BODY_NAMES = [
    'Sun', 'Mercury', 'Venus', 'Earth', 'Mars',
    'Jupiter', 'Saturn', 'Uranus', 'Neptune'
];
const BODY_COLORS = [
    '#ffcc33', '#aaaaaa', '#e6c229', '#3399ff', '#cc4b37',
    '#e0a96d', '#e8d9ac', '#9fe8f0', '#5b7fff'
];

const canvas = document.getElementById('canvas');
const ctx = canvas.getContext('2d');

let width, height;
function resizeCanvas() {
    width = canvas.width = window.innerWidth;
    height = canvas.height = window.innerHeight;
}
window.addEventListener('resize', resizeCanvas);
resizeCanvas();

// View state: pixels per AU, plus a pan offset (in pixels, canvas center-relative)
let pxPerAu = 80.0;
let panX = 0.0;
let panY = 0.0;

let isDragging = false;
let dragStartX = 0, dragStartY = 0;
let panStartX = 0, panStartY = 0;

canvas.addEventListener('mousedown', (e) => {
    isDragging = true;
    dragStartX = e.clientX;
    dragStartY = e.clientY;
    panStartX = panX;
    panStartY = panY;
});
window.addEventListener('mouseup', () => { isDragging = false; });
window.addEventListener('mousemove', (e) => {
    if (!isDragging) return;
    panX = panStartX + (e.clientX - dragStartX);
    panY = panStartY + (e.clientY - dragStartY);
});

// Cursor-centered zoom: keep the AU point under the cursor fixed on screen
canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    const zoomFactor = e.deltaY < 0 ? 1.15 : 1 / 1.15;

    const mouseX = e.clientX - width / 2 - panX;
    const mouseY = e.clientY - height / 2 - panY;
    const auX = mouseX / pxPerAu;
    const auY = mouseY / pxPerAu;

    pxPerAu *= zoomFactor;

    panX = e.clientX - width / 2 - auX * pxPerAu;
    panY = e.clientY - height / 2 - auY * pxPerAu;
}, { passive: false });

// Playback speed control
let speedMultiplier = 1.0;
let paused = false;

document.getElementById('pause-btn').addEventListener('click', () => {
    paused = !paused;
    document.getElementById('pause-btn').textContent = paused ? 'Resume' : 'Pause';
});
document.getElementById('speed-up').addEventListener('click', () => {
    speedMultiplier *= 2;
    document.getElementById('speed-label').textContent = `Speed: ${speedMultiplier}x`;
});
document.getElementById('speed-down').addEventListener('click', () => {
    speedMultiplier /= 2;
    document.getElementById('speed-label').textContent = `Speed: ${speedMultiplier}x`;
});

// Simulated time, in days. Earth completes one orbit (365.256 days) in
// EARTH_ORBIT_REAL_SECONDS seconds of real time at 1x speed - matching
// the pacing described for the original project.
const EARTH_ORBIT_REAL_SECONDS = 10.0;
const EARTH_PERIOD_DAYS = 365.256;
const DAYS_PER_REAL_SECOND = EARTH_PERIOD_DAYS / EARTH_ORBIT_REAL_SECONDS;

let simulatedDays = 0.0;
let lastTimestamp = null;

Module.onRuntimeInitialized = () => {
    const getPosition = Module.cwrap(
        'get_position', null, ['number', 'number', 'number', 'number']
    );
    const getBodyCount = Module.cwrap('get_body_count', 'number', []);
    const getBodyRadiusKm = Module.cwrap('get_body_radius_km', 'number', ['number']);
    const getOrbitalPeriodDays = Module.cwrap('get_orbital_period_days', 'number', ['number']);
    const getMoonCount = Module.cwrap('get_moon_count', 'number', ['number']);
    const getMoonPositionUnit = Module.cwrap(
        'get_moon_position_unit', null, ['number', 'number', 'number', 'number', 'number']
    );

    const bodyCount = getBodyCount();

    // Scratch memory for the two out-parameters (x, y), as doubles.
    // Reused for both planet and moon position calls (calls are sequential,
    // never concurrent, so sharing this scratch space is safe).
    const xPtr = Module._malloc(8);
    const yPtr = Module._malloc(8);

    // Precompute display radii (compressed scale so tiny planets stay
    // visible next to the Sun; adjust freely to taste)
    const displayRadiusPx = [];
    for (let i = 1; i <= bodyCount; i++) {
        const km = getBodyRadiusKm(i);
        displayRadiusPx.push(Math.max(2, Math.log10(km) * 2.2));
    }

    const revolutionCounts = new Array(bodyCount + 1).fill(0);
    const lastAngle = new Array(bodyCount + 1).fill(0);

    // Moon counts per body, fetched once (they never change during the run)
    const moonCounts = [];
    for (let i = 1; i <= bodyCount; i++) {
        moonCounts.push(getMoonCount(i));
    }

    function frame(timestamp) {
        if (lastTimestamp === null) lastTimestamp = timestamp;
        const dtSeconds = (timestamp - lastTimestamp) / 1000.0;
        lastTimestamp = timestamp;

        if (!paused) {
            simulatedDays += dtSeconds * DAYS_PER_REAL_SECOND * speedMultiplier;
        }

        ctx.fillStyle = '#000';
        ctx.fillRect(0, 0, width, height);

        const originX = width / 2 + panX;
        const originY = height / 2 + panY;

        for (let i = 1; i <= bodyCount; i++) {
            Module.HEAPF64.set([0, 0], xPtr / 8);
            getPosition(i, simulatedDays, xPtr, yPtr);
            const xAu = Module.HEAPF64[xPtr / 8];
            const yAu = Module.HEAPF64[yPtr / 8];

            const screenX = originX + xAu * pxPerAu;
            const screenY = originY + yAu * pxPerAu;

            // Track revolutions by counting full angle wraps
            const angle = Math.atan2(yAu, xAu);
            if (i > 1) {
                if (lastAngle[i] > Math.PI / 2 && angle < -Math.PI / 2) {
                    revolutionCounts[i]++;
                }
                lastAngle[i] = angle;
            }

            // Orbit path (skip for the Sun)
            if (i > 1) {
                ctx.beginPath();
                ctx.strokeStyle = 'rgba(255,255,255,0.15)';
                ctx.arc(originX, originY, xAu !== 0 || yAu !== 0
                    ? Math.hypot(xAu, yAu) * pxPerAu : 0, 0, Math.PI * 2);
                ctx.stroke();
            }

            ctx.beginPath();
            ctx.fillStyle = BODY_COLORS[i - 1];
            ctx.arc(screenX, screenY, displayRadiusPx[i - 1], 0, Math.PI * 2);
            ctx.fill();

            if (pxPerAu > 15) {
                ctx.fillStyle = '#ccc';
                ctx.font = '11px monospace';
                ctx.fillText(
                    `${BODY_NAMES[i - 1]}${i > 1 ? ' (' + revolutionCounts[i] + ')' : ''}`,
                    screenX + displayRadiusPx[i - 1] + 4, screenY + 3
                );
            }

            // Moons: rendered at an exaggerated distance from the planet
            // (real moon-planet distances are far too small to show at
            // the same scale as the planets' distances from the Sun).
            // Only drawn once zoomed in enough to avoid visual clutter.
            const nMoons = moonCounts[i - 1];
            if (nMoons > 0 && pxPerAu > 15) {
                for (let m = 1; m <= nMoons; m++) {
                    getMoonPositionUnit(i, m, simulatedDays, xPtr, yPtr);
                    const ux = Module.HEAPF64[xPtr / 8];
                    const uy = Module.HEAPF64[yPtr / 8];

                    const moonOrbitPx = displayRadiusPx[i - 1] + 8 + (m - 1) * 6;
                    const moonX = screenX + ux * moonOrbitPx;
                    const moonY = screenY + uy * moonOrbitPx;

                    ctx.beginPath();
                    ctx.fillStyle = '#bbbbbb';
                    ctx.arc(moonX, moonY, 1.5, 0, Math.PI * 2);
                    ctx.fill();
                }
            }
        }

        requestAnimationFrame(frame);
    }

    requestAnimationFrame(frame);
};