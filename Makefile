SOURCE = slides.md

PANDOC = pandoc
DOT = dot

all: slidy

%.svg : %.dot
	$(DOT) -Tsvg $< -o $@

graphs: $(patsubst %.dot,docs/%.svg,$(wildcard dags/*.dot))

slidy: tdd/slides.md xenomorph/slides.md graphs
	$(PANDOC) -t slidy --standalone --section-divs --highlight-style pygments tdd/slides.md -o docs/tdd.html -V slidy-url="."
	$(PANDOC) -t slidy --standalone --section-divs --highlight-style pygments xenomorph/slides.md -o docs/xenomorph.html -V slidy-url="."

