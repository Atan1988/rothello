library(reticulate)

# use_python('C:/Program Files/Python/Python36', required = T)
# use_virtualenv('c:/venv', required = TRUE)
difflib <- import("difflib")

main <- import_main()

builtins <- import_builtins()
builtins$print('foo')

source_python('inst/add.py')
add(5, 10)

tf <- import('tensorflow')
