import { defineConfig, PlaywrightTestProject } from '@playwright/test';

import {base} from './vite.config.ts'
import inCI from './in-ci.js'
import inProduction from './in-production.js'

type BrowserName = 'chromium' | 'firefox' | 'webkit'

const supportedBrowsers: Array<BrowserName> = inCI ? 
  /* Nix support issues for browsers other than Chromium on x86_64-linux system */
  ['chromium'] : 
  ['chromium', 'firefox', 'webkit'] 

const baseURL = `http://localhost:4173${base}`
const serveMode = inProduction ? 'production' : 'development'

function inDarkMode(project: PlaywrightTestProject): PlaywrightTestProject { 
  if (!project.name) {
    throw Error('project name required')
  }
  return {
    ...project,
    name: `${project.name} (Dark Mode)`,
    use: {...project.use, colorScheme: 'dark'},
  }
}

function inLightMode(project: PlaywrightTestProject): PlaywrightTestProject { 
  if (!project.name) {
    throw Error('project name required')
  }
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
  fullyParallel: !inCI, 
  forbidOnly: inCI,
  globalTimeout: 10 * 60 * 1000,
  reporter: 'html',
  reportSlowTests: {
    max: 0,
    threshold: 10 * 1000,
  },
  retries: inCI ? 2 : 0,
  testDir: './test/ts',
  timeout: 20 * 1000,
  workers: inCI ? 1 : undefined,
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
        ...( 
          browserName === 'firefox' ? 
            [
              desktopDevice(browserName),
            ] :
            [
              desktopDevice(browserName),
              mobileDevice(browserName),
              tabletDevice(browserName),
            ]
        )
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
    reuseExistingServer: !inCI,
    timeout: 30 * 1000,
    url: baseURL,
  },
})
