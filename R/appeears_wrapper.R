

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
appeears_start_session = function(username, password){
  secret = jsonlite::base64_enc(paste(username, password, sep = ":"))
  response = httr::POST(paste0(base_url(), "/login"),
                   httr::add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                               "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                   body = "grant_type=client_credentials")
  token_response = httr::content(response)

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
#' @return Returns a list representing the AppEEARS server response. On a valid request, includes a variable
#' 'task_id' which can be used in further requests to see the status of the task or to retrieve associated bundle.
#'
#' @export
appeears_start_task = function(token, task_name, start_date, end_date, product, layers, type, points, test=FALSE){


  task = '{
          "task_type":'
  if(type=="polygon"){
    task = paste0(task,'"area",')
  }else if(type=="point"){
    task = paste0(task,'"point",')
  }
  task =paste0(task,
                 '"task_name": "',task_name,'",
                 "params":{
                 "dates": [
                 {
                 "startDate": "',start_date,'",
                 "endDate": "',end_date,'"
                 }],
                 "layers": [')

  layers=lapply(layers, function(x){
    paste0('{
      "product": "',product,'",
      "layer": "',x,'"
    }')
  })


  task=paste0(task,paste(layers,collapse=",",sep=""),collapse="")
  if(type=="polygon"){

    task=paste0(task,'],
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

    task = paste0(task,
                   paste0(
                     by(points,1:nrow(points),
                        function(row) paste0("[", row$long, ",",row$lat, "]")),
                     collapse=','
                     )
                   )

    task = paste0(task,']]}}]}}}')

  }else if(type == "point"){
    task=paste0(task,'],
    "coordinates": [')

    task = paste0(task,
                  paste0(
                    by(points,1:nrow(points),
                       function(row) paste0('{"latitude":', row$lat, ',"longitude":',row$long, ',"id":"', row$id, '","category":"', row$category, '"}')),
                    collapse=','
                  )
    )

#    {
#      "latitude": 44.97766115516424,
#      "longitude": -93.26824955642223,
#      "id": "Minneapolis",
#      "category": "Urban"
#    },


    task=paste0(task,']
     }
    }')
  }
  
  if (test){
    print (task)
  }else{
    task = jsonlite::fromJSON(task)
    task = jsonlite::toJSON(task, auto_unbox=TRUE)
    
    auth = paste("Bearer", token$token)
    
    response = httr::POST(paste0(base_url(),"/task"), body = task, encode = "json",
                          httr::add_headers(Authorization = auth, "Content-Type" = "application/json"))
    
    task_response = httr::content(response)
    
    return (task_response)
  }
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
appeears_fetch_bundle = function(task_id, token){
  auth = paste("Bearer", token$token)
  response = httr::GET(paste0(base_url(), "/bundle/", task_id$task_id), httr::add_headers(Authorization = auth))
  bundle_response = httr::content(response)
  bundle_response
}

appeears_download_bundle_files = function(task_id, bundle_id, base_path="./"){

  for (file in bundle_id$files){
    url = paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", task_id$task_id, "/",file$file_id)
    path = paste0(base_path, gsub("/","-",file$file_name))
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
appeears_task_status = function (token, task_id){
  auth = paste("Bearer", token$token)
  response = httr::GET(paste0(base_url(),"/status/", task_id$task_id), httr::add_headers(Authorization = auth))
  status_response = httr::content(response)
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
appeears_task_is_done = function(token, task_id){
  auth = paste("Bearer", token$token)
  response = httr::GET(paste0(base_url(),"/status/", task_id$task_id), httr::add_headers(Authorization = auth))

  code = response$all_headers[[1]]$status

  code == 303
}
