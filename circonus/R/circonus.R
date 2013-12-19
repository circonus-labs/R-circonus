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

circonus.fetch_generic <-
function(obj, checkid, metric, start, end, period, circonus_type = 'numeric', na.rm = FALSE,
                                   verbose = FALSE)
{
  if ( ! is.numeric(start) ) start <- as.numeric(as.POSIXlt(start))
  if ( ! is.numeric(end) ) end <- as.numeric(as.POSIXlt(end))
  metric_encoded <- URLencode(metric, reserved = TRUE)
  url <- paste("https://api.circonus.com/v2/data/",
               checkid, "_", metric_encoded, "?",
               "start=", start, "&",
               "end=", end, "&",
               "type=", circonus_type, "&",
               "period=", period,
               sep = "")
  a = getURL(url, curl=obj$curl, verbose = verbose, ssl.verifypeer = FALSE)
  b = rjson::fromJSON(a)
  if(! is.list(b$data))
    stop(b$message)
  b
}

circonus.fetch_histogram <- 
function(obj, checkid, metric, start, end, period, verbose = FALSE)
{
  epoch <- ISOdatetime(1970,1,1,0,0,0)
  b <- circonus.fetch_generic(obj,checkid,metric,start,end,period,
                              circonus_type='histogram',verbose=verbose)
  tshist <- c()
  tshist$whence <- lapply(b$data, function(x) unlist(x[1]))
  tshist$distributions <- lapply(b$data, function(x) {
    hist <- c()
    hist$bin <- unlist(lapply(names(x[[3]]), as.numeric))
    hist$frequency <- unlist(t(x[[3]]))
    data.frame(hist)
  })
  tshist
}

circonus.fetch_numeric <- 
function(obj, checkid, metric, start, end, period, type = '', na.rm = FALSE,
                                   verbose = FALSE)
{
  epoch <- ISOdatetime(1970,1,1,0,0,0)
  b <- circonus.fetch_generic(obj,checkid,metric,start,end,period,
                              circonus_type='numeric',verbose=verbose)
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
  if(na.rm) {
    z <- val$count * 0
    val$whence <- na.omit(val$whence + z)
    val$count <- na.omit(val$count)
    val$value <- na.omit(val$value)
    val$derivative <- na.omit(val$derivative)
    val$counter <- na.omit(val$counter)
    val$stddev <- na.omit(val$stddev)
    val$derivative_stddev <- na.omit(val$derivative_stddev)
    val$counter_stddev <- na.omit(val$counter_stddev)
  }
  val
}

circonus.nntagg <-
function(x,y)
{
  if(is.na(x$count)) {
    a <- y
  }
  else if(is.na(y$count)) {
    a <- x
  }
  else {
  	aggsd <- function(m,sd,w) {
  	  sqrt(weighted.mean(sd^2, w) + (prod(w)/(sum(w)^2))*((diff(m))^2))
  	}
    a <- c()
    a$count <- x$count + y$count
    a$value <- weighted.mean(c(x$value, y$value), c(x$count, y$count))
    a$derivative <- weighted.mean(c(x$derivative, y$derivative), c(ifelse(is.na(x$derivative),0,1),ifelse(is.na(y$derivative),0,1)))
    a$counter <- weighted.mean(c(x$counter, y$counter), c(ifelse(is.na(x$counter),0,1),ifelse(is.na(y$counter),0,1)))
    a$stddev <- aggsd(c(x$value, y$value), c(x$stddev, y$stddev), c(x$count, y$count))
    a$derivative_stddev <- aggsd(c(x$derivative, y$derivative), c(x$derivative_stddev, y$derivative_stddev), c(1,1))
    a$counter_stddev <- aggsd(c(x$counter, y$counter), c(x$counter_stddev, y$counter_stddev), c(1,1))
  }
  a
}
