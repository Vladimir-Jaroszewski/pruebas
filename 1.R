# Define el contenido del archivo
preambulo <- c(
  "\\usepackage{tikz}",
  "\\usepackage{caption}"
)

# Escribe el contenido en el archivo preámbulo.tex
writeLines(preambulo, "preámbulo.tex")
