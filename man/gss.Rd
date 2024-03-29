% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_documentation.r
\docType{data}
\name{gss}
\alias{gss}
\title{General Social Survey}
\format{
A data frame with 2867 rows and 14 variables
\describe{
\item{aidhouse}{On the whole, do you think it should or should not be the government's
responsibility to provide decent housing for those who can't afford it?
(Definitely Should Be, Probably Should Be, Probably Should Not Be, Definitely Should Not Be)}
\item{partyid}{Combination of questions regarding partisan affiliation and strength of
affiliation.  Results in 7-point scale from Strong Democrat to Strong Republican along with
Other Party affiliation coded separately as 8.}
\item{realinc}{Total family income in constant US dollars.}
\item{aid_scale}{Additive scale of items with same general form as aidhouse, but including
items about: decent standard of living for the old, industry with the help it needs to
grow, decent standard of living for the unemployed, give financial help to university
students and low-income families.  Items were standardized and reversed so higher vales
indicated greater generosity.}
\item{age}{Respondent age.}
\item{sei10}{Socio-economic Status indicator - theoretically ranges from 0 to 1.}
\item{sex}{Binary indicator of respondent sex.}
\item{tax}{Are your federal income taxes too high, about right or too low?}
\item{newsfrom}{Where do you get most of your information about current news events?
(newspapers, magazines, the Internet, books or other printed materials, TV, radio,
government agencies, family, friends, colleagues,  some other source)}
\item{educ}{Total number of years of formal education completed.}
\item{sparts}{Please tell me whether you would like to see more or less
government spending on culture and the arts. Remember, that if you say
"much more" it might require a tax increase to pay for it.
Five-point Scale from Spend Much More to Spend Much Less.}
\item{wtssnr}{Survey Weighting variable}
\item{party3}{Party ID variable that puts leaners, independents and others together in
Other; Strong and moderate Democrats are coded as Democrat while strong and
moderate Republicans are coded Republican.}
\item{childs}{Number of children in respondent's household.}
}
}
\description{
This is a subset of the 2016 US General Social Survey (Smith et. al. 2016).
}
\references{
Smith, Tom W, Peter Marsden, Michael Hout, and Jibum Kim. (2016).
General Social Surveys, 1972-2016 [machine-readable data file] Principal Investigator,
Tom W. Smith; Co-Principal Investigator, Peter V. Marsden; Co-Principal Investigator,
Michael Hout; Sponsored by National Science Foundation. -NORC ed.- Chicago:
NORC at the University of Chicago [producer and distributor]. Data accessed from the
GSS Data Explorer website at \url{https://gss.norc.org/get-the-data}.
}
\keyword{datasets}
