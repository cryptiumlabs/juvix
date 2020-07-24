CONVERT=npx js-yaml
SYNTAXES=juvix
JSON_TARGETS=$(addsuffix .json,$(addprefix syntaxes/,$(SYNTAXES)))
SCOPE_LISTS=$(addsuffix .md,$(addprefix scope-lists/,$(SYNTAXES)))

.PHONY: all publish

all: $(JSON_TARGETS) $(SCOPE_LISTS)

scope-lists/%.md: scope-lists/%.yaml
	scope-lists/refresh.hs md $< $@

scope-lists/%.yaml: syntaxes/%-tmLanguage.yaml
	scope-lists/refresh.hs db $< $@

test: all
	cd test && bash test.sh

%.json: %-tmLanguage.yaml
	$(CONVERT) $< > $@

$(CONVERT):
	npm install js-yaml

publish: all
	vsce publish
