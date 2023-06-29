import lzString from 'lz-string'

export const _compressToEncodedURIComponent =
  lzString.compressToEncodedURIComponent

export const _decompressFromEncodedURIComponent =
  lzString.decompressFromEncodedURIComponent

export function _baseUrl() {
  return import.meta.env.BASE_URL
}
