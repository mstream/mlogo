import { defineConfig, PlaywrightTestProject } from '@playwright/test';

import {base} from './vite.config.js'
import inProduction from './in-production.js'

type BrowserName = 'chromium' | 'firefox' | 'webkit'

const supportedBrowsers: Array<BrowserName> = ['chromium', 'webkit'] 

const baseURL = `http://localhost:4173${base}`
const serveMode = inProduction ? 'production' : 'development'

function inDarkMode(project: PlaywrightTestProject): PlaywrightTestProject { 
  return {
    ...project,
    name: `${project.name} (Dark Mode)`,
    use: {...project.use, colorScheme: 'dark'},
  }
}

function inLightMode(project: PlaywrightTestProject): PlaywrightTestProject { 
  return {
    ...project, 
    name: `${project.name} (Light Mode)`,
    use: {...project.use, colorScheme: 'light'},
  }
}

function desktopDevice(browserName: BrowserName): PlaywrightTestProject {
  return {
    name: `desktop/${browserName}`,
    use: {
      browserName,
      hasTouch: false,
      isMobile: false,
      viewport: {height: 1200, width: 1920}, 
    },
  }
}

function mobileDevice(browserName: BrowserName): PlaywrightTestProject {
  return {
    name: `mobile/${browserName}`,
    use: {
      browserName,
      hasTouch: true,
      isMobile: true,
      viewport: {height: 1280, width: 720}, 
    },
  }
}

function tabletDevice(browserName: BrowserName): PlaywrightTestProject {
  return {
    name: `tablet/${browserName}`,
    use: {
      browserName,
      hasTouch: true,
      isMobile: true,
      viewport: {height: 1600, width: 900}, 
    },
  }
}

export default defineConfig({
  expect: {
    timeout: 5 * 1000,
  },
  fullyParallel: !process.env.CI, 
  forbidOnly: !!process.env.CI,
  globalTimeout: 10 * 60 * 1000,
  reporter: 'html',
  reportSlowTests: {
    max: 0,
    threshold: 10 * 1000,
  },
  retries: process.env.CI ? 2 : 0,
  testDir: './test/ts',
  timeout: 20 * 1000,
  workers: process.env.CI ? 1 : undefined,
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

  projects: supportedBrowsers
    .reduce(
      (acc: Array<PlaywrightTestProject>, browserName) => [
        ...acc,
        desktopDevice(browserName),
        mobileDevice(browserName),
        tabletDevice(browserName),
      ], 
      [],
    )
    .reduce(
      (acc: Array<PlaywrightTestProject>, project: PlaywrightTestProject) => [
        ...acc, 
        inDarkMode(project), 
        inLightMode(project),
      ], 
      [],
    ),

  webServer: {
    command: `npm run serve:${serveMode}`,
    reuseExistingServer: !process.env.CI,
    timeout: 30 * 1000,
    url: baseURL,
  },
})
