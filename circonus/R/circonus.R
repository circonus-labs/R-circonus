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
  epoch <- ISOdatetime(1970,1,1,0,0,0)
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

  extract_column <- function(d, type) {
    unlist(lapply(b$data, function(x) {
      if(is.null(x[[2]]$count) || x[[2]]$count == 0)
        val <- NA
      else
        val <- eval(type)
      val
    }))
  }

  names <- c('count','value','derivative','counter','stddev','derivative_stddev','counter_stddev')
  if(length(which(names==type)) == 1)
    names <- c(type)

  val <- c()
  val$whence <- epoch + unlist(lapply(b$data, function(x) x[[1]]));

  for(name in names) {
    if (name == 'count')
      val$count <- extract_column(b$data, expression(x[[2]]$count))
    else if (name == 'value')
      val$value <- extract_column(b$data, expression(x[[2]]$value))
    else if (name == 'derivative')
      val$derivative <- extract_column(b$data, expression(x[[2]]$derivative))
    else if (name == 'counter')
      val$counter <- extract_column(b$data, expression(x[[2]]$counter))
    else if (name == 'stddev')
      val$stddev <- extract_column(b$data, expression(x[[2]]$stddev))
    else if (name == 'derivative_stddev')
      val$derivative_stddev <- extract_column(b$data, expression(x[[2]]$derivative_stddev))
    else if (name == 'counter_stddev')
      val$counter_stddev <- extract_column(b$data, expression(x[[2]]$counter_stddev))
  }
  val
}
