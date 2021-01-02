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


# --------------------- Interrogation ------------------------------------
# Create Functions For Storing Multiple Git User Details
user <- function(name)
{
  
  req = GET(paste("https://api.github.com/users/", name, "", sep = ""), gtoken)
  reqContent = content(req)
  reqDisplay = jsonlite::fromJSON(jsonlite::toJSON(reqContent))
  return(reqDisplay)
  
}


followers <- function(name)
{
  
  followers = GET(paste("https://api.github.com/users/", name, "/followers?per_page=100", sep = ""), gtoken)
  followersContent = content(followers)
  followersDisplay = jsonlite::fromJSON(jsonlite::toJSON(followersContent))
  return(followersDisplay)
  
}


following <- function(name)
{
  
  following = GET(paste("https://api.github.com/users/", name, "/following?per_page=100", sep = ""), gtoken)
  followingContent = content(following)
  followingDisplay = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  return(followingDisplay)
  
}


repos <- function(name)
{
  
  repos = GET(paste("https://api.github.com/users/", name, "/repos?per_page=150", sep = ""), gtoken)
  reposContent = content(repos)
  reposDisplay = jsonlite::fromJSON(jsonlite::toJSON(reposContent))
  return(reposDisplay)
  
}


# Acquire Details for Andrew Nesbitt
adreq = user("andrew")
adfollowers = followers("andrew")
adfollowing = following("andrew")
adrepos = repos("andrew")

# Look At Data Returned For Reference
adreq
adfollowers$login
adfollowing$login
dim(adrepos)
adrepos$size
adrepos$created_at
adrepos$language

languages <- c(adrepos$language)
languages

followerList = adfollowers$login
followerList = c(followerList)


# Create a data frame to store an array of multiple user's data
# The users will be found by looping through andrew nesbitt's followers
users = c()
users.DF = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer())

# Loop Through Followers to Build users.DF 
for (i in 1:length(followerList)) 
{
  
  followersfollowings = following(followerList[i])
  followersfollowings = followersfollowings$login
  
  for (j in 1:length(followersfollowings))
  {
    
    if (!(is.element(followersfollowings[j], users)))
    {
    
      users[length(users)+1] = followersfollowings[j]
      followersfollowings2 = user(followersfollowings[j])
      
      followersfollowings2_following = followersfollowings2$following
      followersfollowings2_followers = followersfollowings2$followers
      followersfollowings2_repos = followersfollowings2$public_repos
      followersfollowings2_dateCreated = substr(followersfollowings2$created_at, start = 1, stop = 4)
      
      users.DF[nrow(users.DF) + 1, ] = c(followersfollowings[j], followersfollowings2_following, 
                                         followersfollowings2_followers, followersfollowings2_repos,
                                         followersfollowings2_dateCreated)
    
    }
    if (length(users) >= 200){
      break
    }
    next
    
  }
  
  if(length(users) >= 200){
    break
  }
  next
  
}
users.DF
length(users)


table(users.DF$dateCreated)

users.DF_followerTofollowing_relationship <- users.DF[,2:3]
plot(users.DF_followerfollowing_relationship, xlim = c(0,300), ylim = c(0,4000), 
     main = "Relationship between user's followers and followings")
plot(users.DF_followerfollowing_relationship, xlim = c(0,150), ylim = c(0,1000), col = users.DF[,5],
     main = "In Focus Relationship between user's followers and followings coloured by year")


palette("default")
users.DF_reposTofollowers_relationship <- users.DF[,c(4,3)]
plot(users.DF_reposTofollowers_relationship, xlim = c(0,250), ylim = c(0,5000),
     main = "Relationship between number of repos to number of followers")
plot(users.DF_reposTofollowers_relationship, xlim = c(0,250), ylim = c(0,1000), col = users.DF[,5],
     main = "In Focus Relationship between number of repos to number of followers coloured by year")


year_ex2008 = users.DF[users.DF[,5] != 2008,]
year_ex2008

plot(year_ex2008[,c(4,3)], col = year_ex2008[,5], xlim = c(0,100), ylim = c(0,400),
     main = 'Relationship between repos and followers excluding year 2008')

