import { defineConfig } from 'vite'
import { resolve } from 'path'
import { VitePWA } from 'vite-plugin-pwa'
import inProduction from './in-production.js'
import * as url from 'url'

const __dirname = url.fileURLToPath(new URL('.', import.meta.url))

const basePath = inProduction ? '/mlogo/' : '/'

const root = 'build/html'

const pwaIcons = [
  {
    purpose: 'any',
    sizes: '192x192',
    src: 'pwa-192x192.png',
    type: 'image/png',
  },
  {
    purpose: 'any',
    src: 'pwa-512x512.png',
    sizes: '512x512',
    type: 'image/png',
  },
  {
    purpose: 'maskable',
    sizes: '192x192',
    src: 'pwa-192x192.png',
    type: 'image/png',
  },
  {
    purpose: 'maskable',
    src: 'pwa-512x512.png',
    sizes: '512x512',
    type: 'image/png',
  },
]

const pwaShortcuts = [
  {
    "name": "Open Sandbox",
    "short_name": "Sandbox",
    "description": "Evaluate your Logo code",
    "url": "/sandbox.html",
    "icons": [{ "src": "bucket.png", "sizes": "192x192" }]
  }
]

const pwaPlugin = VitePWA({
  manifest: {
    icons: pwaIcons,
    name: 'MLogo',
    scope: basePath,
    shortcuts: pwaShortcuts,
    start_url: basePath,
    theme_color: '#ffffff',
  },
  registerType: 'autoUpdate',
  srcDir: 'build/images'
})


export default defineConfig({
  base: basePath,
  build: {
    emptyOutDir: true,
    outDir: '../../dist',
    rollupOptions: {
      input: {
        home: resolve(__dirname, root, 'index.html'),
        sandbox: resolve(__dirname, root, 'sandbox.html'),
      }
    },
  },
  plugins: [pwaPlugin],
  root,
})

