import { TestInfo } from '@playwright/test'

export default function (testInfo: TestInfo): void {
  // eslint-disable-next-line @typescript-eslint/unbound-method
  const origPath: (name: string) => string = testInfo.snapshotPath

  testInfo.snapshotPath = function (snapshotName: string): string {
    return origPath
      .apply(testInfo, [snapshotName])
      .replace('--linux', '')
      .replace('--darwin', '')
  }
}
