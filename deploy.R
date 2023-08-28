

rsconnect::setAccountInfo(name = 'enrep', 
                          token = '04AF4ADD8F2B1F14CA7CBAB90647E725', 
                          secret = '<SECRET>')

rsconnect::deployApp(
  appDir = "C://Users/Ian Hellman/Documents/GitHub/GOES_Data_Viewer_Shiny_App",
  appFiles = NULL #, you can specify which files to deploy, 
  #or keep this NULL to deploy everything
  ,
  account = "enrep",
  appName = "GOES_Data_Viewer_Shiny_App",
  appTitle = "ENREP GOES Data Viewer")
