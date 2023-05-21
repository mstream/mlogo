const cacheName = 'mlogo-0.40.2'

const urlsToCache = [
  '/',
]

self.addEventListener('install', event => {
  event.waitUntil((async () => {
    const cache = await caches.open(cacheName);
    cache.addAll(urlsToCache)
  })())
})

self.addEventListener('fetch', event => {
  event.respondWith((async () => {
    const cache = await caches.open(cacheName)
    const cachedResponse = await cache.match(event.request);

    if (cachedResponse) {
      return cachedResponse;
    } else {
      try {
        const response = await fetch(event.request);
        cache.put(event.request, response.clone());
        return response;
      } catch (e) { }
    }
  })())
})
