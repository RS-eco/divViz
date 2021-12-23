# Install rsconnect
#install.packages('rsconnect')

# Set the account info from secret variables
#rsconnect::setAccountInfo(name='rs-eco',
#                          token='...',
#                          secret='...')

# Deploy the app
library(rsconnect)
rsconnect::deployApp(appName="divViz", 
                     appTitle="Visualisation tool for biodiversity data")

