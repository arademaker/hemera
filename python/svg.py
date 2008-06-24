import os

def parseToSVG(proof):
	generate_image(proof)
	svg = open_svg()
	
	return svg

def generate_image(proof):
	dot = open("proof.dot", "w" )
	dot.write(proof)
	dot.close()
	
	cmd = "dot -Tsvg proof.dot -o proof.svg"
	
	os.system(cmd)
	
def open_svg():
    remove_header = True
    svg_str = ""
	
    svg_file = open("proof.svg", "r")

    while True:  
        line = svg_file.readline()
        if not line:
            break
		
        svg_str = svg_str + " " + line
        if remove_header == True:
            if svg_str[1:5] != "<svg":
                svg_str = ""
            else:
                remove_header = False
	
    svg_file.close()

    return svg_str
