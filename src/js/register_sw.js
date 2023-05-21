const scope = APP_PATH

if ("serviceWorker" in navigator) {
  navigator.serviceWorker
    .register(new URL("./sw.js", import.meta.url), {
      type: "module",
      scope,
    })
    .then(function() {
      console.log("[index.html] Service Worker Registered")
    })
}
