#' World Values Survey Aggregate Data
#'
#' A subset of data from the second thorugh fifth waves of the World Values Survey.
#'
#' @details We started with waves 2 (Inglehart et. al., 2014a), 3 (Inglehart et. al., 2014b),
#' 4 (Inglehart et. al., 2014c) and 5 (Inglehart et. al., 2014d) of the World Values Survey (WVS).
#' The WVS is a cross-national survey effort aimed at describing the character of value systems
#' around the globe.  From each survey, we capture country and survey year, several demographic
#' variables (Religious Importance, fairness, left-right self-placement, education, income, sex and age)
#' along with some values scales (emancipative values and secular values).  We also capture several
#' questions about the extent to which several controversial actions are morally justifiable.
#' We add data from several other projects to these data.  To measure inequality, we use the Standardized
#' World Income Inequality Data (Solt, 2020).  From this dataset, we capture the Gini Coefficient (both in disposable
#' income and market income, though we tend to use the former in models).  We obtain GDP and population data
#' from the Penn World Tables version 10 (Feenstra et. al., 2015).  We gather data on political rights, civil liberties
#' and freedom status from the Freedom in the World Project (Freedom House, 2020).  We use the civilizations
#' codes from Henderson and Tucker (2001), which were used to test Huntington’s (1996) argument about the “clash of civilizations”.
#' Finally, we get the human development index (HDI) from the United Nations Development Programme (2020).
#' The combined dataset has 237,787 individual observations nested within 84 countries. Most countries appear in
#' only one or two waves (65), but nine appear in three waves and 10 in four waves.
#'
#' We aggregate the variables in the individual dataset by country-wave to produce a more manageable data set.
#' The aggregate dataset has 162 rows and 38 variables.  The variables are as follows:
#'
#' @name wvs
#' @docType data
#' @format A data frame with 162 rows and 26 variables
#' \describe{
#'   \item{country}{Country of respondent residence.}
#'   \item{wave}{Wave of the survey.}
#'   \item{year}{Year of the survey.}
#'   \item{pct_high_rel_imp}{Religious importance is coded as Very, Rather, Not very or Not at all important
#'   in the individual data.  This variable is the proportion of respondents who indicated Very or Rather.}
#'   \item{pct_female}{Proportion of observations identifying as female.}
#'   \item{mean_lr}{Left-right self-placement is coded on a 1 (Left) to 10 (Right) scale in the individual
#'   data.  The \code{mean_lr} variable is the country-wave average of left-right self-placement.}
#'   \item{pct_less_secondary, pct_secondary, pct_some_univ, pct_univ_degree}{In the individual data, education is coded as
#'   Less than secondary, Secondary complete, Some university and University degree or more.  In the aggregate
#'   data, we calculate the proportion of observations in each category.}
#'   \item{pct_low_income, pct_mid_income, pct_high_income}{In the individual data, income is coded in decies (i.e., a 1-10 scale).
#'   In the aggregate data, we calculate the proportion of observations in categories 1-3 (Low), 4-7 (Middle) and 8-10 (High)
#'   categories.}
#'   \item{moral}{In the individual data, we created an additive scale of variables about how justifiable
#'   the following actions are: Illegally claiming government benefits, Avoiding a fare on public transport,
#'   Cheating on taxes, Accepting a bribe, Homosexuality, Divorce, Abortion, Prostitution, Euthanasia,
#'   Suicide on a scale from 1 (Never justifiable) to 10 (Always Justifiable).  In the aggregate data,
#'   we calculate the country-wave average of this scale.}
#'   \item{sacsecval}{Secular Values - opposite of traditional values wherein religion, parent-child
#'   ties, deference to authority and traditional family values are paramount. In the aggregate data, we take the
#'   country-wave average of this scale.}
#'   \item{secpay}{Imagine two secretaries, of the same age, doing practically the same job. One finds out that
#'    the other earns considerably more than she does. The better paid secretary, however, is quicker,
#'    more efficient and more reliable at her job. In your opinion, is it fair or not fair that one secretary
#'    is paid more than the other? The \code{secpay} variable is the proportion of people in each country indicating
#'    that the pay discrepancy is unfair.}
#'   \item{resemaval}{Emancipative Values - preference for gender and racial equality, liberty and personal autonomy.
#'   In the aggregate data, we take the country-wave average of this scale.}
#'   \item{rgdpe}{Expenditure-side real GDP at chained PPPs (in mil. 2017US$).
#'   Useful for making cross-country/cross-time comparisons of relative living standards.
#'   Obtained from Penn World Tables.}
#'   \item{rel_soc}{Dummy variable indicating places where at least 75% of 
#'   respondents identified religion as being important.}
#'   \item{pop}{Population in Millions, obtained from Penn World Tables.}
#'   \item{gdp_cap}{GDP/capita: \code{rgdpe}/\code{pop}.}
#'   \item{gini_disp}{Gini coefficient in terms of disposable income from the SWIID.}
#'   \item{gini_mkt}{Gini coefficient in terms of market prices from the SWIID.}
#'   \item{polrt}{Measure of the violation of political rights from the Freedom in the World Project.
#'   Coded on a scale from 1 (fewest violations) to 7 (most violations).}
#'   \item{civlib}{Measure of the violation of civil liberties from the Freedom in the World Project.
#'   Coded on a scale from 1 (fewest violations) to 7 (most violations).}
#'   \item{democrat}{Using the freedom status variable, we code a country as a democracy
#'   if in the past 15 years it was always at least partly free and was free for at least
#'   50 percent of the time.  This follows the work of Weakliem et. al. (2005).}
#'   \item{civ}{Categories defining the civilization in which each country belongs.
#'   Other=0, African=1, Buddhist=2, Hindu=3, Islamic=4, Japanese=5, Latin American=6, Orthodox=7,
#'   Siinic=8, Western=9.}
#'}
#'
#' @references
#' Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182, available for download at \url{www.ggdc.net/pwt}.
#'
#' Freedom House.  (2020).  Freedom in the World 2020.  New York: Freedom House.
#'
#' Henderson, Errol A. and Richard Tucker. 2001. "Clear and Present Strangers: The Clash of Civilizations and International Conflict." International Studies Quarterly, 45(2):317–338.
#'
#' Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014a. World Values Survey: Round Two - Country-Pooled Datafile Version: \url{www.worldvaluessurvey.org/WVSDocumentationWV2.jsp}. Madrid: JD Systems Institute.
#'
#' Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014b. World Values Survey: Round Three - Country-Pooled Datafile Version: \url{www.worldvaluessurvey.org/WVSDocumentationWV3.jsp}. Madrid: JD Systems Institute.
#'
#' Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014c. World Values Survey: Round Four - Country-Pooled Datafile Version: \url{www.worldvaluessurvey.org/WVSDocumentationWV4.jsp}. Madrid: JD Systems Institute.
#'
#' Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014d. World Values Survey: Round Five - Country-Pooled Datafile Version: \url{www.worldvaluessurvey.org/WVSDocumentationWV5.jsp}. Madrid: JD Systems Institute.
#'
#' Solt, Frederick. 2020. "Measuring Income Inequality Across Countries and Over Time: The Standardized World Income Inequality Database." Social Science Quarterly 101(3):1183-1199. SWIID Version 9.0, October 2020.
#'
#'
#' @keywords datasets
NULL

#' Canadian Election Study
#'
#' These data are a subset of the Canadian Election Study telephone
#' sample (Stephenson et. al. 2020).
#'
#' @name ces
#' @docType data
#' @format A data frame with 2799 rows and 29 variables
#' \describe{
#' \item{vote}{Vote for Parliament - This variable is used to make
#' all of the “Vote for …” variables. These are actual self-reported
#' votes from the post-election study, not campaign-period vote intention.
#' We coded those who indicated did not vote, none, don't know and refused as missing.}
#' \item{gender}{Binary variable indicating respondent sex.}
#' \item{agegrp}{Age Group -  Age is calculated by subtracting the year of birth
#' from the survey year. Then observations are put into age-groups (18-34, 35-54, 55+)}
#' \item{relig}{Religious Affiliation - Respondents are coded into four groups -
#' no religious affiliation/Agnostic, Catholic, Non-Catholic Christians (
#' incl. Anglican, Baptist, Eastern Orthodox, Johova's Witness, Lutheran, Pentecostal,
#' Presbyterian, Protestant, United Church of Canada, Christian, Salvatian Army, Mennonite)
#' and Other (incl. Buddhist, Hindu, Hewish, Muslim, Sikh). We also include an indicator
#'  variable for Catholic vs non-Catholic.}
#' \item{educ}{Educational Attainment coded into three categories HS or Less
#' (incl. No schooling, some elementary, completed elementary, some secondary,
#' completed secondary), Some Post-secondary (incl. some  echnical/community college,
#' completed technical/community college, some university) and Univ Grad
#' (incl. bachelor’s degree, master’s degree, professional degree)}
#' \item{region}{Provinces are coded into four regions:
#' Atlantic (Newfoundland and Labrador, PEI, Nova Scotia, New Brunswick),
#' Quebec, Ontario and the West (Manitoba, Saskatchewan, Alberta and British Columbia)}
#' \item{province}{Province of respondent}
#' \item{pid}{Party with which respondent identifies. These are coded into
#' Liberal, Conservative, NDP, Green, Bloc Quebecois and Other.}
#' \item{retroper}{Retrospective Personal Economic Perceptions - Whether respondent thinks
#' his or her personal economic situation has gotten better, stayed the same or gotten worse
#' in the past year.}
#' \item{retrocan}{Retrospective National Economic Perceptions - Whether respondent thinks
#' Canada's economic situation has gotten better, stayed the same or gotten worse in the past year.}
#' \item{sp_defence}{Respondent's opinion of how much defence spending should change in
#' three categories - Less (much less, less), Stay the same, More (more or much more).}
#' \item{sp_envir}{Respondent's opinion of how much spending on the environment should
#' change in three categories - Less (much less, less), Stay the same, More (more or much more).}
#' \item{immig}{Respondent's opinion about how immigration levels should change -
#' Increase, Stay the same/Don't Know, Decrease}
#' \item{usties}{Respondent's opinion about how ties between Canada and the US should change -
#' Much more distant, Somewhat more distant, Stay the Same/Don't Know, Somewhat closer, Much closer.}
#' \item{jobspriv}{Level of agreement with the following statement - The government should leave
#' it ENTIRELY to the private sector to create jobs: Strongly disagree, Disagree,
#' Don't know, Agree, Strongly agree.}
#' \item{blame}{Level of agreement with the following statement - People who don't
#' get ahead should blame themselves, not the system: Strongly disagree, Disagree,
#' Don't know, Agree, Strongly agree.}
#' \item{poorgap}{How much should be done to reduce the gap between rich and poor in
#' Canada - Much less, Somewhat less, About the same/Don't know, Somewhat more, Much more.}
#' \item{stayhome}{Level of agreement with the following statement - Society would be better
#' off if fewer women worked outside the home: Strongly disagree, Disagree, Don't know,
#' Agree, Strongly agree.}
#' \item{feelgays}{Feeling thermometer for homosexuals.}
#' \item{dowomen}{How much do you think should be done for women: Much less, Somewhat less,
#' About the same/Don't know, Somewhat more, Much more.}
#' \item{leader_lib}{Feeling thermometer for Justin Trudeau, leader of the Liberal Party.}
#' \item{leader_con}{Feeling thermometer for Andrew Scheer, leader of the Conservative Party.}
#' \item{leader_ndp}{Feeling thermometer for Jagmeet Singh, leader of the NDP.}
#' \item{leader_bloc}{Feeling thermometer for Yves-Francois Blanchet, the leader of the Bloc Quebecois.}
#' \item{market}{Market liberalism – additive scale of jobspriv, poorgap and blame variables.}
#' \item{moral}{Moral traditionalism – additive scale of dowomen, stayhome and feelgays.}
#' \item{union}{Whether respondent is a union member - yes or no.}
#' \item{weight_CES}{Weighting variable for the CES.}
#' }
#' @references
#' Stephenson, Laura B,  Allison Harell, Daniel Rubenson, Peter John Loewen.  (2020). "2019 Canadian Election Study - Phone Survey", \url{https://doi.org/10.7910/DVN/8RHLG1}, Harvard Dataverse, V1.
#'
#' @keywords datasets
NULL

#' General Social Survey
#'
#' This is a subset of the 2016 US General Social Survey (Smith et. al. 2016).
#'
#' @name gss
#' @docType data
#' @format A data frame with 2867 rows and 14 variables
#' \describe{
#' \item{aidhouse}{On the whole, do you think it should or should not be the government's
#' responsibility to provide decent housing for those who can't afford it?
#' (Definitely Should Be, Probably Should Be, Probably Should Not Be, Definitely Should Not Be)}
#' \item{partyid}{Combination of questions regarding partisan affiliation and strength of
#' affiliation.  Results in 7-point scale from Strong Democrat to Strong Republican along with
#' Other Party affiliation coded separately as 8.}
#' \item{realinc}{Total family income in constant US dollars.}
#' \item{aid_scale}{Additive scale of items with same general form as aidhouse, but including
#' items about: decent standard of living for the old, industry with the help it needs to
#' grow, decent standard of living for the unemployed, give financial help to university
#' students and low-income families.  Items were standardized and reversed so higher vales
#' indicated greater generosity.}
#' \item{age}{Respondent age.}
#' \item{sei10}{Socio-economic Status indicator - theoretically ranges from 0 to 1.}
#' \item{sex}{Binary indicator of respondent sex.}
#' \item{tax}{Are your federal income taxes too high, about right or too low?}
#' \item{newsfrom}{Where do you get most of your information about current news events?
#' (newspapers, magazines, the Internet, books or other printed materials, TV, radio,
#' government agencies, family, friends, colleagues,  some other source)}
#' \item{educ}{Total number of years of formal education completed.}
#' \item{sparts}{Please tell me whether you would like to see more or less
#' government spending on culture and the arts. Remember, that if you say
#' "much more" it might require a tax increase to pay for it.
#' Five-point Scale from Spend Much More to Spend Much Less.}
#' \item{wtssnr}{Survey Weighting variable}
#' \item{party3}{Party ID variable that puts leaners, independents and others together in
#' Other; Strong and moderate Democrats are coded as Democrat while strong and
#' moderate Republicans are coded Republican.}
#' \item{childs}{Number of children in respondent's household.}
#'}
#'
#' @references
#' Smith, Tom W, Peter Marsden, Michael Hout, and Jibum Kim. (2016).
#' General Social Surveys, 1972-2016 [machine-readable data file] Principal Investigator,
#' Tom W. Smith; Co-Principal Investigator, Peter V. Marsden; Co-Principal Investigator,
#' Michael Hout; Sponsored by National Science Foundation. -NORC ed.- Chicago:
#' NORC at the University of Chicago [producer and distributor]. Data accessed from the
#' GSS Data Explorer website at \url{gssdataexplorer.norc.org}.
#'
#' @keywords datasets
NULL

#' Income and Inequality Data
#'
#' This merges the Gini coefficient measured in disposable income from the
#' Standardized World Income Inequality Data (Solt, 2020) with GDP and population
#' data from the Penn World Tables version 10 (Feenstra et. al., 2015).
#'
#' @name inc_ineq
#' @docType data
#' @format A data frame with 12810 rows and 6 variables
#' \describe{
#' \item{country}{Country Name}
#' \item{year}{Year}
#' \item{rgdpe}{Expenditure-side real GDP at chained PPPs (in mil. 2017 US Dollars).
#'   Useful for making cross-country/cross-time comparisons of relative living standards. [PWT]}
#' \item{pop}{Population in millions. [PWT]}
#' \item{gdp_cap}{GDP divided by population from PWT.}
#' \item{gini}{Gini Coefficient (Disposable Income) [SWIID]}
#' }
#'
#' @references
#' Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182, available for download at \url{www.ggdc.net/pwt}.
#'
#' Solt, Frederick. 2020. "Measuring Income Inequality Across Countries and Over Time: The Standardized World Income Inequality Database." Social Science Quarterly 101(3):1183-1199. SWIID Version 9.0, October 2020.
#'
#' @keywords datasets
NULL

#' Indian Election Data
#'
#' These data are from the International Social Survey Programme:
#' National Identity III survey (ISSP Research Group 2015).  This
#' subset contains only the data from India.
#'
#' @name india
#' @docType data
#' @format A data frame with 1530 rows and 22 variables
#' \describe{
#' \item{patriotism}{Additive scale of level of agreement regarding statements
#' about patriotism (5-point scale Agree Strongly to Disagree Strongly): Strengthens
#' India's place in the world (-), leads to intolerance in India (+), is needed for
#' India to remain united (-), leads to negative attitudes toward immigrants in India (+).
#' All items are standardized before being summed.}
#' \item{imp_roots}{Additive scale of level of agreement regarding statements
#' about importance of the following things for being truly Indian (5-point scale Agree Strongly to Disagree Strongly,
#' all indicators positively associated with the scale): being born in India, having Indian citizenship,
#' having lived in India most of your life, ability to speak Hindi, to be Hindu, to respect India's political institutions
#' and laws, to feel Indian and to have Indian ancestry. All items are standardized and reversed before being summed.}
#' \item{pride_country}{Additive scale of level of agreement regarding statements
#' about pride in the following things about India  (5-point scale Agree Strongly to Disagree Strongly,
#' all indicators positively associated with the scale): the way
#' democracy works, India's political influence in the world,
#' India's economic achievements, its social security system,
#' its scientific and technological achievements, its achievements in sports,
#' its achievements in the arts and literature, India's armed forces,
#' its history, its fair and equal treatment of all groups in society.
#' All items are standardized and reversed before being summed.}
#' \item{country_first}{Additive scale of level of agreement regarding statements
#' about the following things regarding India relationships with other countries
#' (5-point scale Agree Strongly to Disagree Strongly, all indicators positively associated with the scale):
#' India should limit the import of foreign products to protect national economy,
#' India should follow its own interests even if that leads to conflict,
#' foreigners should not be allowed to buy land in India, India's television should give
#' preference to Indian films and programs. All items are standardized and reversed before being summed.}
#' \item{anti_immigration}{Additive scale of level of agreement regarding statements
#' about pride in the following things about immigrants (5-point scale Agree Strongly to Disagree Strongly):
#' immigrants increase crime dates (+), immigrants are generally good for India's economy (-),
#' immigrants take jobs away from people born in India (+),
#' immigrations improve Ind'a s society by bringing new ideas and cultures (-),
#' India's culture is generally undermined by immigrants (-),
#' legal immigrants to India who are not citizens should not have the same rights as Indian citizens (+),
#' India should take stronger measures to exclude illegal immigrants (+),
#' legal immigrants should have equal access to public education as Indian citizens (-).
#' All items standardized before being summed.}
#' \item{educyrs}{Years of formal education, capped at 20.}
#' \item{age}{Respondent age.}
#' \item{sbc}{Dummy indicator for Scheduled or Backaward Caste.}
#' \item{sex}{Binary indicator of respondent gender.}
#' \item{partliv}{Living in a steady relationship with a partner.}
#' \item{religgrp}{Religious group to which respondent belongs.}
#' \item{attend}{Frequency of attendance at religious services.}
#' \item{topbot}{Self-placement in socio-economic status decile.}
#' \item{in_ethn1}{Respondent ethnicity.}
#' \item{hhchildr}{Number of children under 18 in the household.}
#' \item{in_inc}{Income group in local currency.}
#' \item{urbrural}{Urban-rural category of residence.}
#' \item{work}{Ever had paying work (currently, previously, never).}
#' \item{mainstat}{Main current employment status.}
#' \item{union}{Union membership (current, previous, never).}
#' \item{vote_bjp}{Vote for the BJP in most recent election. }
#' \item{vote_le}{Vote turnout in last election.}
#' \item{in_prty}{Party voted for in most recent parliamentary election.}
#' \item{party_lr}{Party voted for in most recent parliamentary election in terms of ideological position.}
#' }
#'
#'
#' @references
#' ISSP Research Group (2015): International Social Survey Programme: National Identity III - ISSP 2013. GESIS Data Archive, Cologne. ZA5950 Data file Version 2.0.0, https://doi.org/10.4232/1.12312
#'
#' @keywords datasets
NULL


#' State Repression Dataset
#' 
#' These data consider the democracy-repression nexus.  While they are different data than used in previous
#' studies, they are similar in spirit to the data used in Poe and Tate (1994) and in 
#' Davenport and Armstrong (20040). 
#' 
#' @name repress
#' @docType data
#' @format A data frame with 1530 rows and 22 variables
#' \describe{
#'   \item{gwno}{Gleditsch and Ward numeric country code}
#'   \item{year}{Year of observation}
#'   \item{pts_s}{Political Terror Scale coding of State Department country reports.}
#'   \item{rgdpe}{Penn World Tables measure of GDP in millions $USD.}
#'   \item{pop}{Population in millions from the Penn World Tables.}
#'   \item{pr}{Freedom House's Political Rights measure (0-40)}
#'   \item{cwar}{Civil War indicator from the UCDP Armed Conflict Database.}
#'   \item{iwar}{Interstate War indicator from the UCDP Armed Conflict Database.}
#'}
#'
#'@references
#' Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer 2015. \sQuote{The Next Generation of the Penn World Table} American Economic Review, 105(10), 3150-3182, available for download at \url{www.ggdc.net/pwt}.
#' 
#' Freedom House.  (2020).  Freedom in the World 2020.  New York: Freedom House.
#' 
#' Gleditsch, Nils Petter; Peter Wallensteen, Mikael Eriksson, Margareta Sollenberg & Havard Strand, 2002. \sQuote{Armed Conflict 1946–2001: A New Dataset}, Journal of Peace Research 39(5): 615–637.
#' 
#' Gibney, Mark, Linda Cornett, Reed Wood, Peter Haschke, Daniel Arnon, Attilio Pisano, Gray Barrett, and Baekkwan Park. 2020. \sQuote{The Political Terror Scale 1976-2019.} Date Retrieved, from the Political Terror Scale website: \url{http://www.politicalterrorscale.org}.
#'
#' @keywords datasets
NULL


#' World Values Survey Religious Importance
#'
#' A subset of data from the second thorugh fifth waves of the World Values Survey measuring
#' religious importance.
#'
#' @details These data come from the same source as the \code{wvs} data.  These are aggregated
#' responses to the question about religious importance by country and religious importance response.  
#' 
#' The  dataset has 224 rows and 4 variables.  The variables are as follows:
#'
#' @name relig_imp
#' @docType data
#' @format A data frame with 224 rows and 4 variables
#' \describe{
#'   \item{country}{Country of respondent residence.}
#'   \item{relig_imp}{Response Category for the religious importance variable: Very Important, 
#'   Rather Important, Not Very Important and Not At All Important.}
#'   \item{n}{Proportion of observation in each country-response category.}
#'   \item{l}{The average value of religious importance on the 1-4 scale.}
#'}
#'
#' @references
#' Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014a. World Values Survey: Round Two - Country-Pooled Datafile Version: \url{www.worldvaluessurvey.org/WVSDocumentationWV2.jsp}. Madrid: JD Systems Institute.
#'
#' Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014b. World Values Survey: Round Three - Country-Pooled Datafile Version: \url{www.worldvaluessurvey.org/WVSDocumentationWV3.jsp}. Madrid: JD Systems Institute.
#'
#' Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014c. World Values Survey: Round Four - Country-Pooled Datafile Version: \url{www.worldvaluessurvey.org/WVSDocumentationWV4.jsp}. Madrid: JD Systems Institute.
#'
#' Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014d. World Values Survey: Round Five - Country-Pooled Datafile Version: \url{www.worldvaluessurvey.org/WVSDocumentationWV5.jsp}. Madrid: JD Systems Institute.
#'
#' @keywords datasets
NULL
