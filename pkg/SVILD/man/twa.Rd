\name{twa}
\alias{twa}




\title{Time Weighted Average}
\description{
	\code{twa} calculate time weighted average using different interpolate methods
}
\usage{
	twa(object, interpolate = c("linear", "leftstep", "rightstep", "spline"), ...)
}

\arguments{
    \item{object}{an object of class \code{SVILD}.}
    \item{interpolate}{defines interpolat method, default: linear.}
    \item{\dots}{ a spline method and see spline() for more detail.}
}

\value{
	a data frame contains:
	\item{ID}{unique ID}
  	\item{TWA }{TWA values}

}

\author{Dongsheng Yang, Anne S. Tang}

\examples{
id = c(17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 21, 21, 21, 21, 21, 21, 21, 
30, 30, 30, 30, 30, 30, 30)

y = c(110, 137, 142, 129, 102, 113, 195, 168, 192, 156, 178, 186, 116, 105, 
111, 86, 113, 115, 106, 113, 115, 106, 97, 103)
	  
time = c("05JUL07:15:10:00", "05JUL07:16:15:00", "05JUL07:17:15:00", 
"05JUL07:18:15:00", "05JUL07:19:15:00", "05JUL07:20:00:00", "05JUL07:21:10:00", 
"05JUL07:21:40:00", "05JUL07:22:10:00", "05JUL07:22:40:00", "10MAY07:08:15:00", 
"10MAY07:09:15:00", "10MAY07:10:15:00", "10MAY07:10:30:00", "10MAY07:11:00:00", 
"10MAY07:11:30:00", "10MAY07:12:20:00", "12JUL07:08:15:00", "12JUL07:08:20:00", 
"12JUL07:09:15:00", "12JUL07:09:20:00", "12JUL07:10:15:00", "12JUL07:10:20:00", 
"12JUL07:11:10:00")
	
a = SVILD(id = id, time = time, y = y, timeFormat = "\%d\%b\%y:\%H:\%M:\%S")
twa(a,interpolate = "linear" )
twa(a,interpolate = "spline", method = "natural", n = 500 )
twa(a,interpolate = "spline", method = "fmm", n = 500 )
}

\keyword{ Statistics }

