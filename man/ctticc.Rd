\name{ctticc}
\alias{Classical Test Theory Item Characteristic Curves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Classical Test Theory Item Characteristic Curves
%%  ~~function to do ... ~~
}
\description{
Item characteristic curves (ICC's) are visual indicators of important attributes of assessment items - most commonly difficulty and discrimination. Assessment specialists who examine ICC's usually do so from within the psychometric framework of either Item Response Theory (IRT) or Rasch modeling. This `R` package provides an extension of this tradition into the Classical Test Theory (CTT) framework. The package has a `psych` dependency that facilitate the estimation of CTT-generated difficulty (_pseudob_) and discrimination (_pseudoa_) "parameters" from a `psych::alpha` object, and then plots the ICCs.

}
\usage{
ctticc(dataframe,
items=dataframe,
plot= "together",
nrow=2,
ncol=3)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{The `dataframe` should contain binary responses from all items comprising the unidimensional scale.}
  \item{item}{The `items` field indicates which columns should be presented visually. Use the concatenate function if you'd like to specify non-sequential items (for example, `c(1,4,7)`) would present ICCs for data columns 1, 4, and 7. By default it is set to plot all the items comprising the dataframe.}
  \item{plot}{The `plot` field has values of `grid`, `separate`, or `together`. If using the `grid` specification, you will also need to specify nrow and ncol.}
  \item{nrow}{If using the `grid` specification, you will also need to specify number of rows.}
  \item{ncol}{If using the `grid` specification, you will also need to specify number of columns.}

  }

  {
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Diego Figueiras (figueirasd1@montclair.edu)

John Kulas (jtkulas@ergreports.com)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(testdata)

#Plotting first 5 items of testdata in the same figure.
ctticc(data, item=1:5)

#Plotting items 10 to 18 of testdata on a grid
ctticc(data, item=10:18, plot="grid", nrow=3, ncol=3)

#Plotting items 10 to 18 of testdata separately
ctticc(data, item=10:18, plot="separate")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
