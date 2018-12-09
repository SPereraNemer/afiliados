# List of packages for session
paquetes = c("knitr","xlsx","mets","stringr")

# Install packages (if not already installed)
instalados <- paquetes %in% installed.packages()
if(length(paquetes[!instalados]) > 0) install.packages(paquetes[!instalados])

# Load packages into session 
lapply(paquetes, require, character.only=TRUE)
