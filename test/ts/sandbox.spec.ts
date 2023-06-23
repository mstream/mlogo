import AxeBuilder from '@axe-core/playwright'
import { test, expect } from '@playwright/test'

test.describe('Sandbox Page', () => {
  test.beforeEach(async ({page}) => {
    await page.goto('sandbox')
  })

  test('has the right title', async ({ page }) => {
    await expect(page).toHaveTitle(/MLogo/)
    await expect(page).toHaveTitle(/Sandbox/)
  })
  
  test('the editor is tall enough', async ({ page }) => {
    const viewportHeight = page.viewportSize()?.height || 0
    const codeInputTextbox = page.getByRole('textbox', {name: 'code input'})
    const codeInputHeight = (await codeInputTextbox.boundingBox())?.height || 0
    expect(codeInputHeight / viewportHeight).toBeGreaterThanOrEqual(1/3)
  })

  test.fixme('has no accessibility issues', async ({ page }) => {
    const accessibilityScanResults = await new AxeBuilder({ page }).analyze()
    expect(accessibilityScanResults.violations).toEqual([])
  })
})

