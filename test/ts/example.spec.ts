import { test, expect } from '@playwright/test'

test('has the right title', async ({ page }) => {
  await page.goto('');
  await expect(page).toHaveTitle(/MLogo/);
});

test('has the right author', async ({ page }) => {
  await page.goto('');
  await expect(page.getByRole('link', {name: 'Maciej Laciak'})).toBeVisible()
});
