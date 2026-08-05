# Install rsconnect
#install.packages('rsconnect')

# Set the account info from secret variables (login to shinyapps.io to see SECRET)
#library(rsconnect)
#rsconnect::connectCloudUser()

# Create a manifest file
rsconnect::writeManifest(appFiles="app.R")

# Check app Dependencies
rsconnect::appDependencies()

# Deploy the app
options(rsconnect.verbose = TRUE)
rsconnect::deployApp(appName="divViz", appTitle="Visualisation tool for biodiversity data", 
                     appFiles=c("app.R", list.files("data", full.names=T), "inst/extdata/art_data.parquet"), forceUpdate=T)
