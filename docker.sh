# linux
docker run --name tk_rstudio \
	-e PASSWORD= -e ROOT=TRUE \
	-d -v $(pwd):/var/repositories/r-peta-ball \
	-p 8787:8787 \
	rocker/tidyverse