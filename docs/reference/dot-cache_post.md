# Cache a post request

This function copies a remote file to a local cache once and makes sure
it is reused.

## Usage

``` r
.cache_post(
  url,
  body,
  as = c("text", "raw", "parsed", "httr"),
  ...,
  .nocache = getOption("cache.disable", default = FALSE),
  .cache = .cache_loc(),
  .stale = Inf
)
```

## Arguments

- url:

  the url to download

- body:

  a named list of POST data

- as:

  how the response is delivered?

- ...:

  Named arguments passed on to
  [`httr::POST`](https://httr.r-lib.org/reference/POST.html)

  `config`

  :   Additional configuration settings such as http authentication
      ([`authenticate()`](https://httr.r-lib.org/reference/authenticate.html)),
      additional headers
      ([`add_headers()`](https://httr.r-lib.org/reference/add_headers.html)),
      cookies
      ([`set_cookies()`](https://httr.r-lib.org/reference/set_cookies.html))
      etc. See
      [`config()`](https://httr.r-lib.org/reference/config.html) for
      full details and list of helpers.

  `...`

  :   Further named parameters, such as `query`, `path`, etc, passed on
      to
      [`modify_url()`](https://httr.r-lib.org/reference/modify_url.html).
      Unnamed parameters will be combined with
      [`config()`](https://httr.r-lib.org/reference/config.html).

  `encode`

  :   If the body is a named list, how should it be encoded? Can be one
      of form (application/x-www-form-urlencoded), multipart,
      (multipart/form-data), or json (application/json).

      For "multipart", list elements can be strings or objects created
      by
      [`upload_file()`](https://httr.r-lib.org/reference/upload_file.html).
      For "form", elements are coerced to strings and escaped, use
      [`I()`](https://rdrr.io/r/base/AsIs.html) to prevent
      double-escaping. For "json", parameters are automatically
      "unboxed" (i.e. length 1 vectors are converted to scalars). To
      preserve a length 1 vector as a vector, wrap in
      [`I()`](https://rdrr.io/r/base/AsIs.html). For "raw", either a
      character or raw vector. You'll need to make sure to set the
      [`content_type()`](https://httr.r-lib.org/reference/content_type.html)
      yourself.

  `handle`

  :   The handle to use with this request. If not supplied, will be
      retrieved and reused from the
      [`handle_pool()`](https://httr.r-lib.org/reference/handle_pool.html)
      based on the scheme, hostname and port of the url. By default httr
      requests to the same scheme/host/port combo. This substantially
      reduces connection time, and ensures that cookies are maintained
      over multiple requests to the same host. See
      [`handle_pool()`](https://httr.r-lib.org/reference/handle_pool.html)
      for more details.

- .nocache:

  an option to defeat the cacheing which can be set globally as
  `options("cache.disable"=TRUE)`

- .cache:

  the location of the cache as a directory. May get its value from
  `getOption("cache.path")` or the default value which is
  `rappdirs::user_cache_dir(<package_name>)`

- .stale:

  the length of time in days before considering cached data as stale.

## Value

the result of the query
