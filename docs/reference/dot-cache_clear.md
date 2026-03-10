# Clear data from the passthrough cache for complex or long running operations

Clear data from the passthrough cache for complex or long running
operations

## Usage

``` r
.cache_clear(.cache = .cache_loc(), .prefix = ".*", interactive = TRUE)
```

## Arguments

- .cache:

  the location of the cache as a directory. May get its value from
  `getOption("cache.path")` or the default value which is
  `rappdirs::user_cache_dir(<package_name>)`

- .prefix:

  (optional) a name of the operation so that you can namespace the
  cached files and do selective clean up operations on them with
  .cache_clear()

- interactive:

  suppress `are you sure?` warning with a FALSE value (defaults to TRUE)

## Value

nothing. called for side effects
