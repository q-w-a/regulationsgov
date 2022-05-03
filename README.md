
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regulationsgov <img src="man/figures/logo.png" align="right" height="139" />

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
First, we do so assuming we have set up the key. See the **Obtaining
Data** vignette for more information on how to use the `endpoint`
argument.

``` r

# retrieve all comments for the docket CMS-2014-0063
comments_CMS_2014_0063 <- get_all_comments(endpoint = "document",
                                           docketId = "CMS-2014-0063")
```

If the key is not set up, then we provide the key with the `key`
argument. Here, you need to replace `DEMO_KEY` with your specific key
unless you are running a very limited number of calls (the rate limit
for `DEMO_KEY` is much lower than an ordinary API key).

``` r


# retrieve all comments for the docket CMS-2014-0063
comments_CMS_2014_0063 <- get_all_comments(endpoint = "document",
                                           docketId = "CMS-2014-0063", 
                                           key = "DEMO_KEY")
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

documents <- regulationsgov::get_all_documents(
  docketId = "FDA-2009-N-0501")


str(documents)
#> 'data.frame':    9 obs. of  33 variables:
#>  $ data_id                                     : chr  "FDA-2009-N-0501-0009" "FDA-2009-N-0501-0008" "FDA-2009-N-0501-0005" "FDA-2009-N-0501-0001" ...
#>  $ data_type                                   : chr  "documents" "documents" "documents" "documents" ...
#>  $ data_links_self                             : chr  "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0009" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0008" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0005" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0001" ...
#>  $ data_allowLateComments                      : chr  "FALSE" "FALSE" "FALSE" "FALSE" ...
#>  $ data_commentEndDate                         : chr  "2017-10-24T03:59:59Z" "2017-08-08T03:59:59Z" NA "2009-12-22T04:59:59Z" ...
#>  $ data_commentStartDate                       : chr  "2017-09-22T04:00:00Z" "2017-06-07T04:00:00Z" NA "2009-10-20T04:00:00Z" ...
#>  $ data_effectiveDate                          : chr  "2017-09-22T04:00:00Z" "2017-06-07T04:00:00Z" "2010-02-26T14:54:01Z" NA ...
#>  $ data_frDocNum                               : chr  "2017-20283" "2017-11821" "2010-04003" "E9-25100" ...
#>  $ data_frVolNum                               : chr  "82 FR 44422" "82 26489" "75 FR 8960" "74FR53746" ...
#>  $ data_ombApproval                            : chr  "0910-0643" NA NA NA ...
#>  $ data_paperLength                            : chr  "0" "0" "0" "0" ...
#>  $ data_paperWidth                             : chr  "0" "0" "0" "0" ...
#>  $ data_startEndPage                           : chr  "44422 - 44424" "26489 - 26492" "8960 - 8963" "53746 - 53749" ...
#>  $ data_agencyId                               : chr  "FDA" "FDA" "FDA" "FDA" ...
#>  $ data_docketId                               : chr  "FDA-2009-N-0501" "FDA-2009-N-0501" "FDA-2009-N-0501" "FDA-2009-N-0501" ...
#>  $ data_documentType                           : chr  "Notice" "Notice" "Notice" "Notice" ...
#>  $ data_format1                                : chr  "pdf" "pdf" "pdf" "pdf" ...
#>  $ data_format2                                : chr  "htm" "htm" "htm" "htm" ...
#>  $ data_size1                                  : chr  "217528" "233578" "59532" "54542" ...
#>  $ data_size2                                  : chr  "15872" "21304" "21308" "18844" ...
#>  $ data_objectId                               : chr  "0900006482b6e6bf" "090000648269fd6f" "0900006480aaf366" "0900006480a4670a" ...
#>  $ data_modifyDate                             : chr  "2017-09-22T13:38:49Z" "2017-06-07T15:04:53Z" "2017-02-09T14:18:30Z" "2017-02-09T14:17:13Z" ...
#>  $ data_originalDocumentId                     : chr  "FDA_FRDOC_0001-7718" "FDA_FRDOC_0001-7471" NA NA ...
#>  $ data_pageCount                              : chr  "3" "4" "4" "4" ...
#>  $ data_postedDate                             : chr  "2017-09-22T04:00:00Z" "2017-06-07T04:00:00Z" "2010-02-26T05:00:00Z" "2009-10-20T04:00:00Z" ...
#>  $ data_receiveDate                            : chr  "2017-09-22T04:00:00Z" "2017-06-07T04:00:00Z" "2010-02-26T05:00:00Z" "2009-10-19T04:00:00Z" ...
#>  $ data_subtype                                : chr  "30 Day Proposed Information Collection" "60 Day Proposed Information Collection" "30 Day Proposed Information Collection" "60 Day Proposed Information Collection" ...
#>  $ data_title                                  : chr  "Agency Information Collection Activities; Submission for Office of\nManagement and Budget Review; Comment Reque"| __truncated__ "Agency Information Collection Activities; Proposed Collection; Comment Request; Third Party Disclosure and Reco"| __truncated__ "Agency Information Collection Activities; Proposals, Submissions, and Approvals: Third Party Disclosure and Rec"| __truncated__ "Agency Information Collection Activities; Proposed Collection; Comment Request; Third Party Disclosure and Reco"| __truncated__ ...
#>  $ data_withdrawn                              : chr  "FALSE" "FALSE" "FALSE" "FALSE" ...
#>  $ data_openForComment                         : chr  "FALSE" "FALSE" "FALSE" "FALSE" ...
#>  $ data_relationships_attachments_links_self   : chr  "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0009/relationships/attachments" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0008/relationships/attachments" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0005/relationships/attachments" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0001/relationships/attachments" ...
#>  $ data_relationships_attachments_links_related: chr  "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0009/attachments" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0008/attachments" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0005/attachments" "https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0001/attachments" ...
#>  $ fileUrl                                     : chr  "https://downloads.regulations.gov/FDA-2009-N-0501-0009/content.pdf,https://downloads.regulations.gov/FDA-2009-N"| __truncated__ "https://downloads.regulations.gov/FDA-2009-N-0501-0008/content.pdf,https://downloads.regulations.gov/FDA-2009-N"| __truncated__ "https://downloads.regulations.gov/FDA-2009-N-0501-0005/content.pdf,https://downloads.regulations.gov/FDA-2009-N"| __truncated__ "https://downloads.regulations.gov/FDA-2009-N-0501-0001/content.pdf,https://downloads.regulations.gov/FDA-2009-N"| __truncated__ ...
```

### Example: Retrieving the Comments Associated with a Specific Docket

In this case, there are only 13 comments associated with docket
`FDA-2009-N-0501`. We can obtain them with the function
`get_all_comments`. You might notice the same individual has submitted
multiple comments, which we also can verify
[here](https://www.regulations.gov/docket/FDA-2009-N-0501/comments).

``` r

comments <- regulationsgov::get_all_comments(endpoint = "document",
                                             docketId ="FDA-2009-N-0501")

comments %>%
  select(data_id, 
         data_title,
         data_stateProvinceRegion) %>%
  str()
#> 'data.frame':    13 obs. of  3 variables:
#>  $ data_id                 : chr  "FDA-2009-N-0501-0002" "FDA-2009-N-0501-0003" "FDA-2009-N-0501-0004" "FDA-2009-N-0501-0013" ...
#>  $ data_title              : chr  "American Frozen Food Institute - Comment " "National Grain and Feed Association - Comment " "The Food Marketing Institute (FMI) - Comment" "Comment from Anonymous" ...
#>  $ data_stateProvinceRegion: chr  NA "DC" "VA" NA ...
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

If we don’t want to download the text but instead want the text included
in the data frame, you can use the `add_text` function. This function
adds a column `text` where each entry contains all the processed text
from any attachments for that document or comment.

``` r

comments <- add_text(comments)

comments %>%
  select(data_id, 
         data_comment,
         text) %>%
  str()
#> 'data.frame':    13 obs. of  3 variables:
#>  $ data_id     : chr  "FDA-2009-N-0501-0002" "FDA-2009-N-0501-0003" "FDA-2009-N-0501-0004" "FDA-2009-N-0501-0013" ...
#>  $ data_comment: chr  "See attachment for comments." "Attached please find the statement of the National Grain and Feed Association in response to the Food and Drug "| __truncated__ NA "In her role as WH Chief Propagandist, Kayleigh McEnany splits her time between praising President Trump and att"| __truncated__ ...
#>  $ text        : chr  "December 18, 2009\n\n[Submitted electronically via www.regulations.gov]\nDivision of Dockets Management (HFA-30"| __truncated__ "December 21, 2009\n\nDivision of Dockets Management (HFA-305)\nFood and Drug Administration\n5630 Fishers Lane\"| __truncated__ "imrr FAI I ~\nFOOD MARKETING INSTITUTE                                         2427           10 JAN >> P4 53\n"| __truncated__ NA ...
```
