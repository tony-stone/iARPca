# Estimating the impact of the implementation of the Ambulance Response Programme on outcomes for individuals who experience a cardiac arrest and are attended by the ambulance service (iARPca)

The Ambulance Response Programme[^1] (ARP) was an initiative established by NHS England to support operational efficiency and improve the performance of ambulance services in England. From February 2015 to November 2017 two major changes were made to ambulance service operations –

* The introduction of additional time to triage 999 calls to enable better dispatch of an appropriate response; and
* a revision of call categories to support provision of responses that are a better fit between urgency, clinical need and appropriate response.

We will examine the impact of implementing ARP’s major changes on the outcomes of individuals who experience a cardiac arrest and are attended by the ambulance service. We will use linked ambulance and hospital data from the Out-of-Hospital Cardiac Arrest Outcomes project[^2] database. This database holds information on individuals who receive care for an out-of-hospital cardiac arrest (OHCA) from ambulance services in England.  For each OHCA event, data on the patient’s demographics, their OHCA event, their out-of-hospital treatment and their outcomes are recorded.

We propose to use two retrospective experimental designs: (i) an uncontrolled before-after study; and (ii) an interrupted time-series study with one “level change” (or “step”) for each of the two ARP major changes. For both study designs we propose two outcome measures: (a) return of spontaneous circulation (ROSC) at hospital handover; and (b) survival to (hospital) discharge. We will examine each ambulance service trust separately (as the date of implementation of the changes varied by service) and provide an overall meta-analysis in the form of a forest plot for each outcome measure. We will use data from ambulance service trusts from one year before the introduction of the first ARP major change to one year after the introduction of the second ARP major change. As the interval between the implementation of the major changes varied by ambulance service trust the periods examined will vary in duration from 31 to 58 months, the total period over which data is required spans 5 years from February 2014 to November 2018 (see [Appendix 1](#Appendix-1:-ARP-implementation-dates)).

In the uncontrolled before-after study we will fit a logistic regression model for each outcome measure with a binary variable indicating before or after (the implementation of the ARP major changes) and covariates for:

* age
* sex
* initial cardiac rhythm
* collapse witnessed (as a proxy for time to emergency ambulance call)
* bystander cardiopulmonary resuscitation (CPR) initiated
* public access defibrillator used by member of public.

We will discard data collected between the implementation of the two ARP major changes, so we will assess the two ARP major changes collectively rather than the impact of the implementation of each major change.

In the interrupted time-series study we will fit a Poisson regression model for each outcome measure using monthly counts of OHCA reported events with subsequent (a) ROSC at hospital handover; and (b) survival to (hospital) discharge. We will exclude data from each month in which an ARP major change was implemented (2 time points in total per model). We will include a linear time trend, adjust for seasonality and include an offset of the (log of the) exposure - the number of reported OHCA events. If we find the data are over-dispersed (for a Poisson model) we will investigate fitting an analogous quasi-Poisson model or a negative binomial model. We will test for autocorrelation by examining the plot of residuals and the partial autocorrelation function. If autocorrelation is detected we will adjust for this using a suitable model that accounts for the presence of autocorrelation. Model convergence can be a problem with generalised linear models, if this is the case we may resort to a generalised least squares model using weights proportional to the number of reported OHCA events.

We will additionally examine the case-mix across the months and assess the stability of the following:

* age
* sex
* initial cardiac rhythm
* collapse witnessed
* bystander CPR initiated
* public access defibrillator used by member of public.

If we detect significant variability in the above we will use direct risk standardisation[^3] to stratify OHCA events and use a generalised least squares model for the time series analysis with the number of events per risk strata as additional covariates in place of the offset/weights.

***
[^1]: https://www.england.nhs.uk/urgent-emergency-care/arp/
[^2]: https://warwick.ac.uk/fac/sci/med/research/ctu/trials/ohcao/
[^3]: https://doi.org/10.1186/1471-2288-13-133

# Appendix 1: ARP implementation dates

ambulance service | notes | Additional triage time implementation date | Revised call categories implementation date | months of data required (including months of change)
----------------- | ----- |:------------------------------------------:|:-------------------------------------------:|:----------------------------------------------------:
East Midlands     |       | 2016-10-04                                 | 2017-07-31                                  | 34
East of England   |       | 2016-10-04                                 | 2017-10-23                                  | 37
London            | A,B   | 20A5-02-01                                 | 2017-11-06                                  | 58
North East        | A     | 2015-10-08                                 | 2017-11-06                                  | 50
North West        |       | 2016-10-04                                 | 2017-08-07                                  | 35
South Central     | A     | 2015-10-19                                 | 2017-11-06                                  | 50
South East Coast  |       | 2016-10-18                                 | 2017-10-20                                  | 38
South West        | A,C,D | 2015-02-01                                 | 2016-04-18                                  | 39
West Midlands     | A,D   | 2015-10-19                                 | 2016-06-13                                  | 33
Yorkshire         | A,D   | 2015-10-19                                 | 2016-04-18                                  | 31

#### Notes:

A. Additional triage time pilot programme service. Six services used the same additional triage time and operational processes from October 2015.

B. London began some pilot work using variable additional triage time in February 2015. London were not consistent in their use of additional triage time until the beginning of the larger pilot programme, see Note A.

C. South West began some pilot work using additional triage time in February 2015. South West were consistent in their use of additional triage time from February 2015.

D. Revised call categories pilot programme site (3 sites). Revised call categories underwent two changes during the pilot period before the categories were finalised. However, these changes to the new categories are unlikely to have affected the categorisation of the highest severity calls (e.g. cardiac arrest). For completeness, we list the dates of changes to these new categories: South West - 2016-10-24; West Midlands - 2016-10-17; Yorkshire - 2016-10-24.

### Overall data required

from | to  | months
:---:|:---:|:---:
2014-02-01 | 2018-11-30 | 58
