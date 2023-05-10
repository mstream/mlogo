function pursExtractor(content) {
  const defaultSelectors = content.match(/[A-Za-z0-9_-]+/g) || []
  const halogenHtmlSelectors = (content.match(/HH\.[A-Za-z]+_?/g) || [])
    .map(selector => selector.slice(3).replace('_', ''))
  return [...defaultSelectors, ...halogenHtmlSelectors] 
}

const extractors = [{
  extractor: pursExtractor,
  extensions: ['purs']
}]

module.exports = {
  content: ['src/purs/**/*.purs'],
  css: ['dist/main.css'],
  extractors,
  output: 'static/',
}
