import * as axe from '@axe-core/playwright'
import { test, expect } from '@playwright/test'

test.describe('Sandbox Page', () => {
  test.beforeEach(async ({page}) => {
    await page.goto('sandbox')
  })

  test('has the right title', async ({ page }) => {
    await expect(page).toHaveTitle(/MLogo/)
    await expect(page).toHaveTitle(/Sandbox/)
  })
   
  test('the canvas is tall enough', async ({ page }) => {
    const viewportHeight = Number(page.viewportSize()?.height)
    const canvasImage = page.getByRole('img', {name: 'canvas'})
    const canvasHeight = Number((await canvasImage.boundingBox())?.height)
    expect(canvasHeight / viewportHeight).toBeGreaterThanOrEqual(1/3)
  })
 
  test('the editor is tall enough', async ({ page }) => {
    const viewportHeight = Number(page.viewportSize()?.height)
    const codeInputTextbox = page.getByRole('textbox', {name: 'code input'})
    const codeInputHeight = Number((await codeInputTextbox.boundingBox())?.height) 
    expect(codeInputHeight / viewportHeight).toBeGreaterThanOrEqual(1/3)
  })

  test('the editor is responsive', async ({ page }) => {
    const programCode = 'forward 10'
    const codeInputTextbox = page.getByRole('textbox', {name: 'code input'})
    const innerTextbox = codeInputTextbox.getByRole('textbox')
    await innerTextbox.fill(programCode)
    expect(codeInputTextbox).toContainText(programCode)
  })

  test.fixme('has no accessibility issues', async ({ page }) => {
    // @ts-expect-error: types mismatch which does not cause any problems
    const axeBuilder: axe.AxeBuilder = new axe.AxeBuilder({ page })
    const accessibilityScanResults = await axeBuilder.analyze()
    expect(accessibilityScanResults.violations).toEqual([])
  })
})

