import { defineConfig } from 'vite'
import { VitePWA } from 'vite-plugin-pwa'
import inProduction from './in-production.js'

const basePath = inProduction ? '/mlogo/' : '/'

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

const pwaPlugin = VitePWA({
  manifest: {
    name: 'MLogo',
    scope: basePath,
    start_url: basePath,
    theme_color: '#ffffff',
    icons: pwaIcons,
  },
  registerType: 'autoUpdate',
  srcDir: 'build/images'
})


export default defineConfig({
  base: basePath,
  build: {
    emptyOutDir: true,
    outDir: '../../dist',
  },
  plugins: [pwaPlugin],
  root: 'build/html',
})

