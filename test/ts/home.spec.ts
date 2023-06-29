import * as axe from '@axe-core/playwright'
import { test, expect } from '@playwright/test'

test.describe('Home Page', () => {
  test.beforeEach(async ({page}) => {
    await page.goto('')
  })

  test('has the right title', async ({ page }) => {
    await expect(page).toHaveTitle(/MLogo/)
  })

  test('has a link to the author\'s page', async ({ page }) => {
    const authorPageLink = page.getByRole('link', {name: 'author'})
    await expect(authorPageLink).toBeVisible()
  })

  test('has no accessibility issues', async ({ page }) => {
    // @ts-expect-error: types mismatch which does not cause any problems
    const axeBuilder: axe.AxeBuilder = new axe.AxeBuilder({ page }) 
    const accessibilityScanResults = await axeBuilder.analyze()
    expect(accessibilityScanResults.violations).toEqual([])
  })
})

