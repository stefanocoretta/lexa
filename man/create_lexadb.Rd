% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{create_lexadb}
\alias{create_lexadb}
\title{Create a new Lexa database}
\usage{
create_lexadb(parent = ".", name)
}
\arguments{
\item{parent}{Parent directory (default is current working directory).}

\item{name}{Name of the Lexa database (\verb{_lexadb} will be appended to the
name).}
}
\value{
Nothing. Used for its side effects.
}
\description{
Create a new Lexa database
}
\examples{
\dontrun{
create_lexadb(parent = "./", name = "my_new")
}
}
