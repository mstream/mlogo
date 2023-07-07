import * as axe from '@axe-core/playwright'
import { test, expect, Page } from '@playwright/test'

async function getCanvasImage(page: Page) {
  return page.getByRole('img', {name: 'canvas'})
}

async function getCodeInputTextbox(page : Page) {
  return page.getByRole('textbox', {name: 'code input'})
}

async function getExamplesTab(page: Page) {
  return page.getByRole('button', {name: 'examples'})
}

async function getExampleFigure(page: Page) {
  return page.getByRole('figure')
}

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
    const canvasImage = await getCanvasImage(page)
    const canvasHeight = Number((await canvasImage.boundingBox())?.height)
    expect(canvasHeight / viewportHeight).toBeGreaterThanOrEqual(1/3)
  })
 
  test('the editor is tall enough', async ({ page }) => {
    const viewportHeight = Number(page.viewportSize()?.height)
    const codeInputTextbox = await getCodeInputTextbox(page)
    const codeInputHeight = Number((await codeInputTextbox.boundingBox())?.height) 
    expect(codeInputHeight / viewportHeight).toBeGreaterThanOrEqual(1/3)
  })

  test('the editor is responsive to input', async ({ page }) => {
    const programCode = 'forward 10'
    const codeInputTextbox = await getCodeInputTextbox(page)
    const innerTextbox = codeInputTextbox.getByRole('textbox')
    await innerTextbox.fill(programCode)
    await expect(codeInputTextbox).toContainText(programCode)
  })

  test('the canvas is responsive to input', async ({ page }) => {
    const programCode = 'forward 10'
    const canvasImage = await getCanvasImage(page)
    const codeInputTextbox = await getCodeInputTextbox(page)
    const innerTextbox = codeInputTextbox.getByRole('textbox')

    expect(await canvasImage.screenshot()).toMatchSnapshot(
      'empty-canvas.svg', 
      {maxDiffPixelRatio: 0},
    )
    await innerTextbox.fill(programCode)
    expect(await canvasImage.screenshot()).toMatchSnapshot(
      'canvas-after-forward-10.svg', 
      {maxDiffPixelRatio: 0}
    )
  })
  
  test('the editor is responsive to examples', async ({ page }) => {
    const codeInputTextbox = await getCodeInputTextbox(page)
    const examplesTab = await getExamplesTab(page)
    await examplesTab.click()
    const firstExampleFigure = (await getExampleFigure(page)).first()
    const firstExampleTryButton = firstExampleFigure.getByRole('button')
    const firstExampleCode = (await firstExampleFigure.getByRole('code').allTextContents())[0]
    await firstExampleTryButton.click()
    await expect(codeInputTextbox).toContainText(firstExampleCode)
  })
  
  test('the canvas is responsive to examples', async ({ page }) => {
    const canvasImage = await getCanvasImage(page)
    const examplesTab = await getExamplesTab(page)
    await examplesTab.click()
    const firstExampleFigure = (await getExampleFigure(page)).first()
    const firstExampleTryButton = firstExampleFigure.getByRole('button')
    expect(await canvasImage.screenshot()).toMatchSnapshot(
      'empty-canvas.svg', 
      {maxDiffPixelRatio: 0},
    )
    await firstExampleTryButton.click()
    expect(await canvasImage.screenshot()).toMatchSnapshot(
      'canvas-after-applying-first-example.svg', 
      {maxDiffPixelRatio: 0}
    )
  })

  test.fixme('has no accessibility issues', async ({ page }) => {
    // @ts-expect-error: types mismatch which does not cause any problems
    const axeBuilder: axe.AxeBuilder = new axe.AxeBuilder({ page })
    const accessibilityScanResults = await axeBuilder.analyze()
    expect(accessibilityScanResults.violations).toEqual([])
  })
})

