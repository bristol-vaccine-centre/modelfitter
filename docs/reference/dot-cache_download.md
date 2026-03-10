# Download a file into a local cache.

This function copies a remote file to a local cache once and makes sure
it is reused.

## Usage

``` r
.cache_download(
  url,
  ...,
  .nocache = getOption("cache.disable", default = FALSE),
  .cache = .cache_loc(),
  .stale = Inf,
  .extn = NULL
)
```

## Arguments

- url:

  the url to download

- ...:

  Named arguments passed on to
  [`utils::download.file`](https://rdrr.io/r/utils/download.file.html)

  `method`

  :   Method to be used for downloading files. Current download methods
      are `"internal"`, `"libcurl"`, `"wget"`, `"curl"` and `"wininet"`
      (Windows only), and there is a value `"auto"`: see ‘Details’ and
      ‘Note’.

      The method can also be set through the option
      `"download.file.method"`: see
      [`options()`](https://rdrr.io/r/base/options.html).

  `quiet`

  :   If `TRUE`, suppress status messages (if any), and the progress
      bar.

  `mode`

  :   character. The mode with which to write the file. Useful values
      are `"w"`, `"wb"` (binary), `"a"` (append) and `"ab"`. Not used
      for methods `"wget"` and `"curl"`. See also ‘Details’, notably
      about using `"wb"` for Windows.

  `cacheOK`

  :   logical. Is a server-side cached value acceptable?

  `extra`

  :   character vector of additional command-line arguments for the
      `"wget"` and `"curl"` methods.

  `headers`

  :   named character vector of additional HTTP headers to use in
      HTTP\[S\] requests. It is ignored for non-HTTP\[S\] URLs. The
      `User-Agent` header taken from the `HTTPUserAgent` option (see
      [`options`](https://rdrr.io/r/base/options.html)) is automatically
      used as the first header.

  `...`

  :   allow additional arguments to be passed, unused.

- .nocache:

  an option to defeat the cacheing which can be set globally as
  `options("cache.disable"=TRUE)`

- .cache:

  the location of the cache as a directory. May get its value from
  `getOption("cache.path")` or the default value which is
  `rappdirs::user_cache_dir(<package_name>)`

- .stale:

  the length of time in days before considering cached data as stale.

- .extn:

  the file name extension

## Value

the path to the downloaded file
