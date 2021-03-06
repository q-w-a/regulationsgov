% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download-all.R
\name{download_all}
\alias{download_all}
\title{Download All Files}
\usage{
download_all(data_frame, dest, format = NULL, ...)
}
\arguments{
\item{data_frame}{a data frame of the form created by \code{\link[=get_all_documents]{get_all_documents()}}
or \code{\link[=get_all_comments]{get_all_comments()}}}

\item{dest}{destination directory where you want the files to be downloaded.
Paths can be relative or absolute.}

\item{format}{a character vector of the file formats to download.
The default is \code{NULL} in which case all files are downloaded.}

\item{...}{additional parameters to be passed to \code{\link[utils:download.file]{utils::download.file()}}.
For example, \code{quiet = TRUE} can be used to suppress the messages produced
by \code{\link[utils:download.file]{utils::download.file()}}.}
}
\description{
Download all the files corresponding to the given file URLS
of documents or comments.
}
