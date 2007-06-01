
all : graph.py graph.tex

graph.py : graph.nw
	notangle -Rgraph.py graph.nw > graph.py

graph.tex : graph.nw
	noweave -delay graph.nw > graph.tex

clean:
	rm graph.{py,tex,aux,log,pdf}


