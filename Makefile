SOURCE = slides.md
STYLE = style.html
THEME = night

PANDOC = pandoc
DOT = dot

all: reveal

clean:
	rm -f slides.html

postprocess: reveal
	sed -i'' -e 's/reveal.min/reveal/' slides.html
	sed -i'' -e "s/simple.css/$(THEME).css/" slides.html

reveal: $(SOURCE) $(STYLE)
	$(PANDOC) -t html5 --template=template-revealjs.html --standalone --section-divs --variable theme="beige" --variable transition="linear" slides.md -o docs/slides.html
	#$(PANDOC) -f markdown --smart -t revealjs -V theme=$(THEME) --include-in-header=$(STYLE) -s $(SOURCE) -o slides.html

%.svg : %.dot
	$(DOT) -Tsvg $< -o $@

graphs: $(patsubst %.dot,docs/%.svg,$(wildcard dags/*.dot))

slidy: slides.md graphs
	$(PANDOC) -t slidy --standalone --section-divs --highlight-style pygments slides.md -o docs/slides.slidy.html

index: slidy
	cp docs/slides.slidy.html docs/index.html
