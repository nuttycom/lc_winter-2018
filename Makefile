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
	$(PANDOC) -t html5 --template=template-revealjs.html --standalone --section-divs --variable theme="beige" --variable transition="linear" slides.md -o slides.html
	#$(PANDOC) -f markdown --smart -t revealjs -V theme=$(THEME) --include-in-header=$(STYLE) -s $(SOURCE) -o slides.html

%.svg : %.dot
	$(DOT) -Tsvg $< -o $@

graphs: dags/tasks1.svg dags/tasks2.svg dags/tasks3.svg dags/tasks4.svg

slidy: slides.md graphs
	$(PANDOC) -t slidy --standalone --section-divs --highlight-style pygments slides.md -o slides.slidy.html

index: slidy
	cp slides.slidy.html index.html
