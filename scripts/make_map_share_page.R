library(rmarkdown)
# Temporary fix to replace docs page with one just including the datawrapper version of this map
# tempfix
# Render page
rmarkdown::render('scripts/providermap.Rmd', 
                  output_dir = "docs",
                  output_file = 'providers_by_county2.html')
