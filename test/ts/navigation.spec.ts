import { test, expect, Page, Locator } from '@playwright/test'

function isViewportMobileSize(page: Page): boolean {
  const viewportWidth = page.viewportSize()?.width
  return viewportWidth != null && viewportWidth < 1024
}

async function forAllTabs(
  page: Page, 
  cb: (tabLink: Locator) => Promise<void>,
): Promise<void> {
  const tabNames = ['Sandbox']
  const tabLinks = tabNames.map(tabName => 
    page.getByRole('link', {name: tabName})
  )
 
  for (const tabLink of tabLinks) {
    await cb(tabLink) 
  }
}

test.describe('Navigation', () => {
  test.beforeEach(async ({page}) => {
    await page.goto('')
  })

  test('all tabs visible', async ({ page }) => {
    test.skip(isViewportMobileSize(page), 'only on desktops')
    await forAllTabs(page, tabLink => expect(tabLink).toBeVisible())
  })

  test('has the burger menu visible', async ({ page }) => {
    test.skip(!isViewportMobileSize(page), 'only on mobile devices')
    await forAllTabs(page, tabLink => expect(tabLink).toBeHidden())
    const burgerMenuLink = page.getByRole('button', {name:'menu'})
    await expect(burgerMenuLink).toBeVisible()
    await burgerMenuLink.click()
    await forAllTabs(page, tabLink => expect(tabLink).toBeVisible())
    await burgerMenuLink.click()
    await forAllTabs(page, tabLink => expect(tabLink).toBeHidden())
  })
})

