const CACHE_NAME = 'go-sleep-v2';
const ASSETS = [
  './',
  '__SCRIPT_URL__',
  '__MANIFEST_URL__',
  '__STYLE_URL__',
  '__FAVICON_URL__',
  '__FONT_400_URL__',
  '__FONT_400I_URL__',
  '__FONT_700_URL__',
  '__BORING_SGF_URL__',
  '__EXCITING_SGF_URL__'
];

async function cacheMissingAssets() {
  const cache = await caches.open(CACHE_NAME);

  await Promise.all(ASSETS.map(async (asset) => {
    const request = new Request(asset);
    const cached = await cache.match(request);
    if (!cached) {
      await cache.add(request);
    }
  }));
}

self.addEventListener('install', (event) => {
  event.waitUntil(cacheMissingAssets());
  self.skipWaiting();
});

self.addEventListener('activate', (event) => {
  event.waitUntil(
    caches.keys().then((keys) =>
      Promise.all(keys.filter((key) => key !== CACHE_NAME).map((key) => caches.delete(key)))
    )
  );
  self.clients.claim();
});

async function networkFirst(request) {
  const cache = await caches.open(CACHE_NAME);

  try {
    const response = await fetch(request, { cache: 'no-cache' });
    if (response && response.ok) {
      await cache.put(request, response.clone());
    }
    return response;
  } catch (error) {
    const cached = await cache.match(request);
    if (cached) return cached;
    throw error;
  }
}

async function cacheFirst(request) {
  const cached = await caches.match(request);
  if (cached) return cached;

  const response = await fetch(request);
  if (response && response.ok) {
    const cache = await caches.open(CACHE_NAME);
    await cache.put(request, response.clone());
  }
  return response;
}

self.addEventListener('fetch', (event) => {
  if (event.request.method !== 'GET') return;

  if (event.request.mode === 'navigate') {
    event.respondWith(networkFirst(event.request));
    return;
  }

  event.respondWith(cacheFirst(event.request));
});
