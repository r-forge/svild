\name{SVILD}

\alias{SVILD}
\alias{print.SVILD}
\alias{summary.SVILD}
\alias{plot.SVILD}
\alias{SVILD.default}
\alias{plot}
\alias{summary}


\title{Summaries of Variability of Irregularly spaced Longitudinal Data}
\description{Read raw data into R and create a list to store all variables of patients}

\usage{

	SVILD(id, ...)

	\method{SVILD}{default}(id, time,y, timeFormat = "\%d\%b\%y:\%H:\%M:\%S", ...)
	\method{print}{SVILD}(x, ...)
	\method{summary}{SVILD}(object, ...)
	\method{plot}{SVILD}(object, n=12, ...)
}

\arguments{
  \item{id}{a numeric vector of patients.}
  \item{time}{a date time vector.}
  \item{y}{a numeric vector of response glucose.}
  \item{timeFormat}{format of date time used.}
  \item{object}{an object of class \code{SVILD}.}
  \item{n}{number of patients plotted.}
  \item{x}{an object}
  \item{\dots}{not used.}
}

\value{
  An object of class is a list including basic elements:


	\item{ID}{unique ID}
	\item{y}{response y}
	\item{lag.y}{lagged y values}
	\item{diff.y}{difference between two sequence time}
	\item{fmttime}{formatted date time}
	\item{time}{rescaled number of time}
	\item{lag.time}{lagged time}
	\item{diff.time}{difference between two sequence time}
}

\author{Dongsheng Yang, Anne S. Tang}

\note{Additional indices will add soon}


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
	
mylist = SVILD(id = id, time = time, y = y, timeFormat = "\%d\%b\%y:\%H:\%M:\%S")
print(mylist)
summary(mylist)
plot(mylist, n = 5)

}

\keyword{ Basics }

