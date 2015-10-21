# Load packages
library(knitr)
library(rmarkdown)

# Create .pdf and .html files
markdownToHTML('StatInference.md', 'StatInference.html', options=c("use_xhml"))
render("StatInference.Rmd")
