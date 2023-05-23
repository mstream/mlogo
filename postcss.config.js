import purgeCssPlugin from '@fullhuman/postcss-purgecss'
import inProduction from './in-production.js'

const productionPlugins = [
  purgeCssPlugin({
    content: ['src/purs/**/*.purs'],
    defaultExtractor(content) {
      const defaultSelectors = content.match(/[A-Za-z0-9_-]+/g) || []
      const halogenHtmlSelectors = (content.match(/HH\.[A-Za-z]+_?/g) || [])
        .map(selector => selector.slice(3).replace('_', ''))
      return [...defaultSelectors, ...halogenHtmlSelectors]
    }
  })
]

export default {
  plugins: inProduction ? productionPlugins : []
}
