
#' Get Data from AppEEARS
#'
#' Handles the process from start to finish of requesting data from AppEEARS and downloading the files. This function is merely
#' a coordinating function for the other subordinate functions available through this package.
#' Please note that because AppEEARS serves data asynchronously, this function must wait an indeterminate amount of time for the
#' job submitted to the AppEEARS server to finish and process. If this does not fit your use case, then you must write your own
#' coordinating function.
#'
#' @param username Username with which to login
#'
#' @param password Password with which to login
#' 
#' @param task_name User specified name to give the task
#'
#' @param start_date Used in conjunction with end_date to specify a date range for the search. Should be a string in MM-DD-YYYY
#' format
#'
#' @param end_date Used in conjunction with start_date to specify a date range for the search. Should be a string in MM-DD-YYYY
#' format
#'
#' @param product Name of the data product to specify in the request
#' A list of available products is available here: https://lpdaacsvc.cr.usgs.gov/appeears/products
#' Takes a string in the format product_name.version_number
#'
#' @param layers layer names to pull from the product as specified in the product parameter. A list of layers
#' available with each product can be found here: https://lpdaacsvc.cr.usgs.gov/appeears/products
#' Takes a vecotor of strings, all of which should perfectly match the layer names specified in the above URL.
#' 
#' @param type Takes 'polygon' or 'point' as input. Specify if the request is for an area or a series of points.
#' 
#' @param points Data frame containing the following columns: lat, long, id, category Used to specify either the boundaries
#' of a polygone or a series of points depending on the input of the 'type' paramater. lat/long correspond with a series of 
#' spatial coordinates. id is an arbitrary identifier for each point and category seems to be an arbitrary string required by
#' AppEEARs but doesn't otherwise do anything, AFAIK.
#' 
#' @param base_path Directory to direct file downloads. Defaults to the working path. Paths relative to the working path can be
#' used with the following noation "./" Be sure to terminate the path with a backslash, e.g. "./my-data-files"
#' 
#' @param wait_time integer greater than or equal to 30. Specifies the number of seconds to wait between checks to see if the 
#' data bundle is ready after submitting a job. Values less than 30 are set to 30 seconds. Defaults to 60 seconds.
#'
#' @return Returns a list containing a success code, 1 on success and 0 on failure, along with a response message indicating the
#' issue if an error occurred
#'
#' @export
appeears_get_data <- function(username, password, task_name, start_date, end_date, product, layers, type, points, base_path="./", wait_time=60){
  
  if(wait_time < 30){
    wait_time <- 30
  }
  
  status <- tryCatch({
    token <- appeears_start_session(username,password)
    if(is.null(token$token)){
      stop("Credential bad")
    }
    
  },error=function(msg){
    return(c(0,"Credentials invalid"))
  })
  
  if(!is.null(status)){
    return(status)
  }
  
  status <- tryCatch({
    task <- appeears_start_task(token,task_name, start_date, end_date, product, layers, type, points)
    if(is.null(task$task_id)){
      stop("Invalid task")
    } 
    
    
  },error=function(msg){
    return(c(0,task$message))
  })
  
  if(!is.null(status)){
    return(status)
  }  
  
  
  
  while(!appeears_task_is_done(token,task)){
    Sys.sleep(wait_time)
  }
  
  status <- tryCatch({
    
    bundle <- appeears_fetch_bundle(task,token)
    
    if(is.null(bundle$files)){
      stop("Invalid bundle")
    }
    appeears_download_bundle_files(task, bundle, base_path)
    
    
  },error=function(msg){
    return(c(0,
             paste0("Error downloading/writing files.Task ID: ",task$task_id)
           ))
  }
  )
  
  if(!is.null(status)){
    return(status)
  }  
  

  return(c(1,"Successfully downloaded files"))
  
}


#' Start AppEEARS Session
#'
#' Provides an authentication token given a username/password valid with the NASA EarthDATA system.
#' Authentication token can be used to make further calls to the AppEEARS server.
#'
#' @param username Username with which to login
#'
#' @param password Password with which to login
#'
#' @return Returns a list representing the response from the AppEEARS server. Includes a variable 'token', necessary
#' for making further requests
#'
#' @export
appeears_start_session <- function(username,password){

  secret <- jsonlite::base64_enc(paste(username, password, sep = ":"))
  response <- httr::POST(paste0(base_url(), "/login"),
                   httr::add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                               "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                   body = "grant_type=client_credentials")
  token_response <- httr::content(response)

  token_response
}


#' Start Task
#'
#' Executes a task on the AppEEARS server for later retrieval.
#' Currently this only accomodates area search with a fixed set of points for the vector. Also, although multiple
#' layers can be specified they must all belong to the same product. So there is room for this to grow.
#'
#' @param token An authentication token associated with the submitted task, as per the appeears_start_session function.
#' Expects a list with a a variable, 'token'
#'
#' @param task_name User specified name to give the task
#'
#' @param start_date Used in conjunction with end_date to specify a date range for the search. Should be a string in MM-DD-YYYY
#' format
#'
#' @param end_date Used in conjunction with start_date to specify a date range for the search. Should be a string in MM-DD-YYYY
#' format
#'
#' @param product Name of the data product to specify in the request
#' A list of available products is available here: https://lpdaacsvc.cr.usgs.gov/appeears/products
#' Takes a string in the format product_name.version_number
#'
#' @param layers layer names to pull from the product as specified in the product parameter. A list of layers
#' available with each product can be found here: https://lpdaacsvc.cr.usgs.gov/appeears/products
#' Takes a vecotor of strings, all of which should perfectly match the layer names specified in the above URL.
#' 
#' @param type Takes 'polygon' or 'point' as input. Specify if the request is for an area or a series of points.
#' 
#' @param points Data frame containing the following columns: lat, long, id, category Used to specify either the boundaries
#' of a polygone or a series of points depending on the input of the 'type' paramater. lat/long correspond with a series of 
#' spatial coordinates. id is an arbitrary identifier for each point and category seems to be an arbitrary string required by
#' AppEEARs but doesn't otherwise do anything, AFAIK.
#'
#' @return Returns a list representing the AppEEARS server response. On a valid request, includes a variable
#' 'task_id' which can be used in further requests to see the status of the task or to retrieve associated bundle.
#'
#' @export
appeears_start_task <- function(token,task_name,start_date,end_date,product,layers, type, points){


  task <- '{
          "task_type":'
  if(type=="polygon"){
    task <- paste0(task,'"area",')
  }else if(type=="point"){
    task <- paste0(task,'"point",')
  }
  task <-paste0(task,
                 '"task_name": "',task_name,'",
                 "params":{
                 "dates": [
                 {
                 "startDate": "',start_date,'",
                 "endDate": "',end_date,'"
                 }],
                 "layers": [')

  layers<-lapply(layers, function(x){
    paste0('{
      "product": "',product,'",
      "layer": "',x,'"
    }')
  })


  task<-paste0(task,paste(layers,collapse=",",sep=""),collapse="")
  if(type=="polygon"){

    task<-paste0(task,'],
      "output":
                 {
                 "format":
                 {
                 "type": "geotiff"
                 },
                 "projection": "albers_weld_conus"
                 },
                 "geo":
                 {
                 "type": "FeatureCollection",
                 "fileName": "User-Drawn-Polygon",
                 "features": [
                 {
                 "type": "Feature",
                 "properties":
                 {},
                 "geometry":
                 {
                 "type": "Polygon",
                 "coordinates": [
                 [')

    task <- paste0(task,
                   paste0(
                     by(points,1:nrow(points),
                        function(row) paste0("[", row$long, ",",row$lat, "]")),
                     collapse=','
                     )
                   )

    task <- paste0(task,']]}}]}}}')

  }else if(type == "point"){
    task<-paste0(task,'],
    "coordinates": [')

    task <- paste0(task,
                  paste0(
                    by(points,1:nrow(points),
                       function(row) paste0('{"latitude":', row$lat, ',"longitude":',row$long, ',"id":"', row$id, '","category":"', row$category, '"}')),
                    collapse=','
                  )
    )


    task<-paste0(task,']
     }
    }')
  }


  task <- jsonlite::fromJSON(task)
  task <- jsonlite::toJSON(task, auto_unbox=TRUE)

  auth <- paste("Bearer", token$token)

  response <- httr::POST(paste0(base_url(),"/task"), body = task, encode = "json",
                   httr::add_headers(Authorization = auth, "Content-Type" = "application/json"))

  task_response <- httr::content(response)
  task_response

}


#' Fetch Bundle
#'
#' Returns the bundle associated with a specified task.
#'
#'
#' @param task_id A task id as per the response given when executing the appeears_start_task function. Expects
#' a list which includes a variable, 'task_id'.
#'
#' @param token An authentication token associated with the submitted task, as per the appeears_start_session function.
#' Expects a list with a a variable, 'token'
#'
#' @return A list representing the AppEEARS server response, which should include a set of bundle information, including
#' all files/file ids associated with the bundle.
#'
#' @export
appeears_fetch_bundle <- function(task_id, token){
  auth <- paste("Bearer", token$token)
  response <- httr::GET(paste0(base_url(), "/bundle/", task_id$task_id), httr::add_headers(Authorization = auth))
  bundle_response <- httr::content(response)
  bundle_response
}


#' Download Bundle Files
#' 
#' Takes an AppEEARS bundle object and downloads all the files associated with that bundle to a local directory.
#' 
#' @param task_id A task id as per the response given when executing the appeears_start_task function. Expects
#' a list which includes a variable, 'task_id'.
#' @param bundle Bundle object obtained from appeears_fetch_bundle function containing a list of all files to download
#' and other information about the data
#' @param base_path Directory to direct file downloads. Defaults to the working path. Paths relative to the working path can be
#' used with the following noation "./" Be sure to terminate the path with a backslash, e.g. "./my-data-files"
#' 
#' @export
appeears_download_bundle_files <- function(task_id, bundle, base_path="./"){

  for (file in bundle$files){
    url <- paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", task_id$task_id, "/",file$file_id)
    path <- paste0(base_path, gsub("/","-",file$file_name))
    download.file(url,destfile=path,method="libcurl", mode="wb")
  }

}



#' Get Task Status
#'
#' Returns information about the status of a particular task. This allows a script to make decisions about what to do
#' given the state of on ongoing task but doesn't provide information about whether or not the task is complete. See
#' 'Is Task Done' for info on that.
#'
#' @param token An authentication token associated with the submitted task, as per the appeears_start_session function.
#' Expects a list with a a variable, 'token'
#'
#' @param task_id A task id as per the response given when executing the appeears_start_task function. Expects
#' a list which includes a variable, 'task_id'.
#'
#' @return A list representing the AppEEARS server response which should include the 'percent complete' status of the
#' task in the task processing workflow.
#'
#' @export
#'
appeears_task_status <- function (token, task_id){
  auth <- paste("Bearer", token$token)
  response <- httr::GET(paste0(base_url(),"/status/", task_id$task_id), httr::add_headers(Authorization = auth))
  status_response <- httr::content(response)
  status_response
}

#' Is Task Done
#'
#' Returns true/false whether or not a given task is done and ready for retrieval.
#'
#' @param token An authentication token associated with the submitted task, as per the appeears_start_session function.
#' Expects a list with a a variable, 'token'
#'
#' @param task_id A task id as per the response given when executing the appeears_start_task function. Expects
#' a list which includes a variable, 'task_id'.
#'
#' @return A boolean representing whether or not the task is complete.
#' @export
#'
appeears_task_is_done <- function(token, task_id){
  auth <- paste("Bearer", token$token)
  response <- httr::GET(paste0(base_url(),"/status/", task_id$task_id), httr::add_headers(Authorization = auth))

  code <- response$all_headers[[1]]$status

  code == 303
}
