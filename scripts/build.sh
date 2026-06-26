#!/usr/bin/env bash
set -euo pipefail

hash_file() {
  sha256sum "$1" | cut -c1-12
}

boring_hash=$(hash_file public/boring.sgf)
exciting_hash=$(hash_file public/exciting.sgf)
font_400_hash=$(hash_file public/recursive-sans-csl-400.woff2)
font_400i_hash=$(hash_file public/recursive-sans-csl-400i.woff2)
font_700_hash=$(hash_file public/recursive-sans-csl-700.woff2)
favicon_hash=$(hash_file public/favicon.svg)
manifest_hash=$(hash_file manifest.json)

cat > src/AssetPaths.elm <<EOF
module AssetPaths exposing (boringSgf, excitingSgf)


boringSgf : String
boringSgf =
    "public/boring.sgf?v=${boring_hash}"


excitingSgf : String
excitingSgf =
    "public/exciting.sgf?v=${exciting_hash}"
EOF

elm make src/Main.elm --output=go-sleep.js "$@"

js_hash=$(hash_file go-sleep.js)

sed \
  -e "s|__FONT_400_URL__|recursive-sans-csl-400.woff2?v=${font_400_hash}|g" \
  -e "s|__FONT_400I_URL__|recursive-sans-csl-400i.woff2?v=${font_400i_hash}|g" \
  -e "s|__FONT_700_URL__|recursive-sans-csl-700.woff2?v=${font_700_hash}|g" \
  public/style.template.css > public/style.css

style_hash=$(hash_file public/style.css)

sed \
  -e "s|__STYLE_URL__|public/style.css?v=${style_hash}|g" \
  -e "s|__FAVICON_URL__|public/favicon.svg?v=${favicon_hash}|g" \
  -e "s|__MANIFEST_URL__|manifest.json?v=${manifest_hash}|g" \
  -e "s|__SCRIPT_URL__|go-sleep.js?v=${js_hash}|g" \
  index.template.html > index.html

sed \
  -e "s|__STYLE_URL__|./public/style.css?v=${style_hash}|g" \
  -e "s|__FAVICON_URL__|./public/favicon.svg?v=${favicon_hash}|g" \
  -e "s|__MANIFEST_URL__|./manifest.json?v=${manifest_hash}|g" \
  -e "s|__SCRIPT_URL__|./go-sleep.js?v=${js_hash}|g" \
  -e "s|__FONT_400_URL__|./public/recursive-sans-csl-400.woff2?v=${font_400_hash}|g" \
  -e "s|__FONT_400I_URL__|./public/recursive-sans-csl-400i.woff2?v=${font_400i_hash}|g" \
  -e "s|__FONT_700_URL__|./public/recursive-sans-csl-700.woff2?v=${font_700_hash}|g" \
  -e "s|__BORING_SGF_URL__|./public/boring.sgf?v=${boring_hash}|g" \
  -e "s|__EXCITING_SGF_URL__|./public/exciting.sgf?v=${exciting_hash}|g" \
  service-worker.template.js > service-worker.js
