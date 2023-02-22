# Install rsconnect
#install.packages('rsconnect')

# Set the account info from secret variables
#rsconnect::setAccountInfo(name='rs-eco', token='...', secret='...')

# Deploy the app
library(rsconnect)
rsconnect::deployApp(account="rs-eco", server = 'shinyapps.io',
                     #appDir = 'C:\\Users\\Documents\\R\\SHINY', appFiles=c('app.R','sales.RData','www/custom.css'),
                     appName="divViz_de", appTitle="Visualisations Tool für Biodiversitäts-Daten")
