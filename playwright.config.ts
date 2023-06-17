import { defineConfig } from '@playwright/test';

import {base} from './vite.config.js'
import inProduction from './in-production.js'

const baseURL = `http://localhost:4173${base}`
const serveMode = inProduction ? 'production' : 'development'

export default defineConfig({
  testDir: './test/ts',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: 'html',
  use: {
    baseURL,
    screenshot: 'only-on-failure',
    trace: {
      mode: 'retain-on-failure',
      screenshots: true,
      snapshots: true,
      sources: true,
    },
  },

  projects: [
    {
      name: 'Desktop/Chromium',
      use: {
        browserName: 'chromium',
        hasTouch: false,
        isMobile: false,
        viewport: {height: 1200, width: 1920}, 
      },
    },
  ],

   webServer: {
     command: `npm run serve:${serveMode}`,
     reuseExistingServer: !process.env.CI,
     url: baseURL,
   },
})
