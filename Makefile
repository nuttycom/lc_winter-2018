SOURCE = slides.md

PANDOC = pandoc
DOT = dot

all: slidy

%.svg : %.dot
	$(DOT) -Tsvg $< -o $@

graphs: $(patsubst %.dot,docs/%.svg,$(wildcard dags/*.dot))

slidy: slides.md graphs
	$(PANDOC) -t slidy --standalone --section-divs --highlight-style pygments slides.md -o docs/slides.slidy.html -V slidy-url="."

index: slidy
	cp docs/slides.slidy.html docs/index.html
