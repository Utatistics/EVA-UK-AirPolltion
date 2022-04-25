library(reticulate)

# reference 
"https://rstudio.github.io/reticulate/"
"https://qiita.com/yamano357/items/9319d4b073d82a261ed8#%E4%BA%8B%E5%89%8D%E6%BA%96%E5%82%99"


# calling python script 
python_path <- Sys.which("python")
use_python(python_path)
source_python("repository/python/sample.py")

sample_func(2)
