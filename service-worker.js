const CACHE_NAME = 'go-sleep-v1';
const ASSETS = [
  './',
  './index.html',
  './go-sleep.js',
  './manifest.json',
  './public/style.css',
  './public/favicon.svg',
  './public/boring.sgf',
  './public/exciting.sgf',
  './public/recursive-sans-csl-400.woff2',
  './public/recursive-sans-csl-400i.woff2',
  './public/recursive-sans-csl-700.woff2'
];

self.addEventListener('install', (event) => {
  event.waitUntil(caches.open(CACHE_NAME).then((cache) => cache.addAll(ASSETS)));
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

self.addEventListener('fetch', (event) => {
  if (event.request.method !== 'GET') return;

  event.respondWith(
    caches.match(event.request).then((cached) =>
      cached || fetch(event.request).then((response) => {
        const copy = response.clone();
        caches.open(CACHE_NAME).then((cache) => cache.put(event.request, copy));
        return response;
      })
    )
  );
});
