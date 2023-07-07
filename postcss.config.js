import purgeCssPlugin from '@fullhuman/postcss-purgecss'
import inProduction from './in-production.js'

const productionPlugins = [
  purgeCssPlugin({
    content: ['src/html/**/*.html', 'src/purs/**/*.purs'],
    defaultExtractor(content) {
      const defaultSelectors = content.match(/[A-Za-z0-9_-]+/g) ?? []

      const halogenHtmlSelectors = (
        content.match(/HH\.[A-Za-z]+_?/g) ?? []
      ).map((selector) => selector.slice(3).replace('_', ''))

      const htmlSelectors = (
        content.match(/<[A-Za-z]+?\/>/g) ?? []
      ).map((selector) =>
        selector.replace('<', '').replace('>', '').replace('/', ''),
      )

      return [
        ...defaultSelectors,
        ...halogenHtmlSelectors,
        ...htmlSelectors,
      ]
    },
  }),
]

export default {
  plugins: inProduction ? productionPlugins : [],
}
