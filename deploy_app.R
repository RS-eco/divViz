# Install rsconnect
#install.packages('rsconnect')

# Set the account info from secret variables
rsconnect::setAccountInfo(name='rs-eco', token='73966D252CAB8CE04D582EDCAEF789AE', secret='...') # Secret is available from shinyapps.io after Login

# Deploy the app
library(rsconnect)
rsconnect::deployApp(account="rs-eco", server = 'shinyapps.io',
                     #appDir = 'C:\\Users\\Documents\\R\\SHINY', appFiles=c('app.R','sales.RData','www/custom.css'),
                     appName="divViz", appTitle="Visualisation tool for biodiversity data")
