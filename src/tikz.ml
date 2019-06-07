let preamble_begin =
  "\\documentclass[cropped,10pt]{standalone}\n\
   \\usepackage{tikz}\n\
   \\usetikzlibrary[positioning,arrows,arrows.meta,shapes,calc,spy,intersections,shadows,bending]\n\
   \\usetikzlibrary[decorations.text,decorations.pathmorphing,decorations.markings]\n"


let preamble_end =
  "\\pagestyle{empty}\n\\thispagestyle{empty}\n\\begin{document}\n\\begin{tikzpicture}\n"


let grid_code =
  "\\draw[help lines,step=1,thin,gray!20] (0,0) grid (100, 100);\n\
   \\draw[help lines,step=5,gray!20,very thick] (0,0) grid (100, 100);\n\
   \\draw[help lines,step=10,gray!40,ultra thick] (0,0) grid (100, 100);\n\
   \\foreach \\j in {10,15,..., 50}\n\
   \\node at (\\j,85) [fill=white,text=red!80!black] {\\j};\n\
   \\foreach \\j in {10,15,..., 95}\n\
   \\node at (23,\\j) [fill=white,text=green!80!black] {\\j};\n"


let preamble_template ?(grid = false) header =
  let g = if grid then grid_code else "" in
  preamble_begin ^ header ^ preamble_end ^ g


let end_template = "\\end{tikzpicture}\n\\end{document}\n"

let include_fig =
  Printf.sprintf "\\node at (0,0) [above right, inner sep=0em] {\\input{%s}};"


let font_cmbright = "\\usepackage{cmbright}\n"

let font_helvetica =
  "\\usepackage[scaled=1.1]{helvet}\n\
   \\usepackage{sfmath}\n\
   \\renewcommand{\\familydefault}{\\sfdefault}\n"
