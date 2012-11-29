circonus <-
function(obj, ...) UseMethod("circonus")
circonus.default <-
function(apikey, ...)
{
  curl <- getCurlHandle()

  curlSetOpt( .opts = list(httpheader = c(
    'X-Circonus-App-Name' = "R",
    'X-Circonus-Auth-Token' = apikey,
    'Accept' = "application/json")), curl=curl)

  obj <- list(apikey = apikey, curl = curl)
  obj$call <- match.call()
  class(obj) <- "circonus"
  obj
}

circonus.fetch_numeric <-
function(obj, checkid, metric, start, end, period, type = '',
                                   verbose = FALSE)
{
  if ( ! is.numeric(start) ) start <- as.numeric(as.POSIXlt(start))
  if ( ! is.numeric(end) ) end <- as.numeric(as.POSIXlt(end))
  metric_encoded <- URLencode(metric, reserved = TRUE)
  url <- paste("https://api.circonus.com/v2/data/",
               checkid, "_", metric_encoded, "?",
               "start=", start, "&",
               "end=", end, "&",
               "period=", period,
               sep = "")
  a = getURL(url, curl=obj$curl, verbose = verbose)
  b = rjson::fromJSON(a)
  if(! is.list(b$data))
    stop(b$message)
  if ( type == '' ) type <- expression(x[[2]])
  else if (type == 'count') type <- expression(x[[2]]$count)
  else if (type == 'value') type <- expression(x[[2]]$value)
  else if (type == 'derive') type <- expression(x[[2]]$derive)
  else if (type == 'counter') type <- expression(x[[2]]$counter)
  else if (type == 'stddev') type <- expression(x[[2]]$stddev)
  else if (type == 'derive_stddev') type <- expression(x[[2]]$derive_stddev)
  else if (type == 'counter_stddev') type <- expression(x[[2]]$counter_stddev)
  lapply(b$data, function(x) {
    if(is.null(x[[2]]))
      val <- NA
    else
      val <- eval(type)
    c(x[[1]], val)
  })
}
