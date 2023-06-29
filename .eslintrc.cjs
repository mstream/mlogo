/* eslint-env node */
module.exports = {
  env: {
    node: true,
  },
  extends: [
    'eslint:recommended', 
    'plugin:@typescript-eslint/recommended',
    'plugin:@typescript-eslint/recommended-requiring-type-checking',
    'plugin:@typescript-eslint/strict',
    'plugin:playwright/playwright-test',
  ],
  ignorePatterns: [
    'postcss.config.js',
    'build/',
    'dist/',
    'node_modules/',
    'output/',
    'output-es/',
  ],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    project: true,
  },
  plugins: [
    '@typescript-eslint'
  ],
  root: true,
  rules: {
    '@typescript-eslint/array-type': [
      'error', 
      { default: 'generic', readonly: 'generic' },
    ],
    'playwright/no-skipped-test': 'off',
  },
}
