
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regulationsgov

<!-- badges: start -->

[![R-CMD-check](https://github.com/q-w-a/regulationsgov/workflows/R-CMD-check/badge.svg)](https://github.com/q-w-a/regulationsgov/actions)
<!-- badges: end -->

The package regulationsgov is designed to enable users to access and
engage with textual data from the [Regulations.gov
API](https://open.gsa.gov/api/regulationsgov/).

## Installation

You can install the development version of regulationsgov from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
remotes::install_github("q-w-a/regulationsgov")
```

## First Steps

To access the [Regulations.gov
API](https://open.gsa.gov/api/regulationsgov/), you will need to obtain
an API key
[here](https://open.gsa.gov/api/regulationsgov/#getting-started).

Once you have this key, you can provide it to every time you call one of
the functions requiring authentication using the `key` argument, but it
may be easiest to set it up as an environmental variable. You can do
this with the following function.

``` r

library(regulationsgov)

set_datagov_key("PUT_KEY_HERE")
```

If you run `set_datagov_key`, you don’t need to provide the key as an
argument to functions in this package. You only need to run it once.
However, if you choose to not set it up, you can just provide the key to
the functions requiring authentication.

### Example: Retrieving Comments for a Specific Docket

For example, here we retrieve all comment metadata, including the
download links, for the docket
[CMS-2014-0063](https://www.regulations.gov/docket/CMS-2014-0063).
First, we do so assuming we have set up the key.

``` r

# retrieve all comments for the docket CMS-2014-0063
comments_CMS_2014_0063 <- get_all_comments(docketId = "CMS-2014-0063")
```

If the key is not set up, then we provide the key with the `key`
argument. Here, you need to replace `DEMO_KEY` with your specific key
unless you are running a very limited number of calls (the rate limit
for `DEMO_KEY` is much lower than an ordinary API key).

``` r


# retrieve all comments for the docket CMS-2014-0063
comments_CMS_2014_0063 <- get_all_comments(docketId = "CMS-2014-0063", key = "DEMO_KEY")
```

After running this command, if needed you can access it as an
environmental variable with

``` r

Sys.getenv("DATA_GOV_KEY")
```

but this will likely be unnecessary for use of the functions in this
package.

### Example: Retrieving the Documents Associated with a Specific Docket

Here, we obtain document and metadata for all documents associated with
the docket
[FDA-2009-N-0501](https://www.regulations.gov/docket/FDA-2009-N-0501).

``` r

library(dplyr)

documents <- regulationsgov::get_all_documents(docketId ="FDA-2009-N-0501")


glimpse(documents)
#> Rows: 9
#> Columns: 33
#> $ data_id                                      <chr> "FDA-2009-N-0501-0009", "…
#> $ data_type                                    <chr> "documents", "documents",…
#> $ data_links_self                              <chr> "https://api.regulations.…
#> $ data_allowLateComments                       <chr> "FALSE", "FALSE", "FALSE"…
#> $ data_commentEndDate                          <chr> "2017-10-24T03:59:59Z", "…
#> $ data_commentStartDate                        <chr> "2017-09-22T04:00:00Z", "…
#> $ data_effectiveDate                           <chr> "2017-09-22T04:00:00Z", "…
#> $ data_frDocNum                                <chr> "2017-20283", "2017-11821…
#> $ data_frVolNum                                <chr> "82 FR 44422", "82 26489"…
#> $ data_ombApproval                             <chr> "0910-0643", NA, NA, NA, …
#> $ data_paperLength                             <chr> "0", "0", "0", "0", "0", …
#> $ data_paperWidth                              <chr> "0", "0", "0", "0", "0", …
#> $ data_startEndPage                            <chr> "44422 - 44424", "26489 -…
#> $ data_agencyId                                <chr> "FDA", "FDA", "FDA", "FDA…
#> $ data_docketId                                <chr> "FDA-2009-N-0501", "FDA-2…
#> $ data_documentType                            <chr> "Notice", "Notice", "Noti…
#> $ data_format1                                 <chr> "pdf", "pdf", "pdf", "pdf…
#> $ data_format2                                 <chr> "htm", "htm", "htm", "htm…
#> $ data_size1                                   <chr> "217528", "233578", "5953…
#> $ data_size2                                   <chr> "15872", "21304", "21308"…
#> $ data_objectId                                <chr> "0900006482b6e6bf", "0900…
#> $ data_modifyDate                              <chr> "2017-09-22T13:38:49Z", "…
#> $ data_originalDocumentId                      <chr> "FDA_FRDOC_0001-7718", "F…
#> $ data_pageCount                               <chr> "3", "4", "4", "4", "3", …
#> $ data_postedDate                              <chr> "2017-09-22T04:00:00Z", "…
#> $ data_receiveDate                             <chr> "2017-09-22T04:00:00Z", "…
#> $ data_subtype                                 <chr> "30 Day Proposed Informat…
#> $ data_title                                   <chr> "Agency Information Colle…
#> $ data_withdrawn                               <chr> "FALSE", "FALSE", "FALSE"…
#> $ data_openForComment                          <chr> "FALSE", "FALSE", "FALSE"…
#> $ data_relationships_attachments_links_self    <chr> "https://api.regulations.…
#> $ data_relationships_attachments_links_related <chr> "https://api.regulations.…
#> $ fileUrl                                      <chr> "https://downloads.regula…
```

### Example: Retrieving the Comments Associated with a Specific Docket

In this case, there are only 13 comments associated with docket
`FDA-2009-N-0501`. We can obtain them with the function
`get_all_comments`. You might notice the same individual has submitted
multiple comments, which we also can verify
[here](https://www.regulations.gov/docket/FDA-2009-N-0501/comments).

``` r

comments <- regulationsgov::get_all_comments(docketId ="FDA-2009-N-0501")

comments %>%
  select(data_id, 
         data_title,
         data_stateProvinceRegion) %>%
  glimpse()
#> Rows: 13
#> Columns: 3
#> $ data_id                  <chr> "FDA-2009-N-0501-0002", "FDA-2009-N-0501-0003…
#> $ data_title               <chr> "American Frozen Food Institute - Comment ", …
#> $ data_stateProvinceRegion <chr> NA, "DC", "VA", NA, "DC", "DC", "DC", NA, NA,…
```

Of particular interest may be the `fileUrl` column, which allows us to
download the text.

For example, to download any attachments for the comments and place them
in the directory `comments_FDA_2009_N_0501`, we can run

``` r

download_all(data_frame = comments, 
             dest = "../comments_FDA_2009_N_0501", 
             quiet = TRUE)
```

and to download the documents themselves to the directory
`docs_FDA_2009_N_0501` we can run the following.

``` r

download_all(data_frame = documents, 
             dest = "../docs_FDA_2009_N_0501", 
             quiet = TRUE)
```
