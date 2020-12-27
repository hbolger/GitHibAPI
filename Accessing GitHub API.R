#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "hbolger_AccessingAPI",
                   key = "3d2e2c363e1a69ea51bd",
                   secret = "354e8bf079cf1a47afbd44a05f15314947dbf441")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 


str(gitDF)
gitDF$created_at
gitDF$teams_url


##############


#My GitHub account is fairly limited as I have not used it extensively throughout the year

#Preparing data for Rozenn Dahyot           
rdreq = GET("https://api.github.com/users/roznn", gtoken)
rdreqContent = content(rdreq)
rdfollowers = GET("https://api.github.com/users/roznn/followers", gtoken)
rdfollowersContent = content(rdfollowers)
rdrepos = GET("https://api.github.com/users/roznn/repos", gtoken)
rdreposContent = content(rdrepos)

#Translate all into data frames:
reqDisplay = jsonlite::fromJSON(jsonlite::toJSON(rdreqContent))
reqDisplay$
followersDisplay = jsonlite::fromJSON(jsonlite::toJSON(rdfollowersContent))
reposDisplay = jsonlite::fromJSON(jsonlite::toJSON(rdreposContent))

reqDisplay
followersDisplay
reposDisplay



