const esbuild = require('esbuild');
const ElmPlugin = require('esbuild-plugin-elm');

esbuild.build({
  entryPoints: ['elm/index.js'],
  bundle: true,
  outdir: 'static/elm',
  watch: process.argv.includes('--watch'),
  plugins: [
    ElmPlugin(),
  ],
}).catch(e => (console.error(e), process.exit(1)))
