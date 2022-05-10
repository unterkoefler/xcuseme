ifneq ($(wildcard IHP/.*),)
IHP = IHP/lib/IHP
else
IHP = $(shell dirname $$(which RunDevServer))/../lib/IHP
endif

CSS_FILES += static/app.css

JS_FILES += static/elm/index.js

include ${IHP}/Makefile.dist

static/elm/index.js:
		NODE_ENV=production yarn install --frozen-lockfile
		NODE_ENV=production node build.js
