PEW RESEARCH CENTER 
April 19-23, 2017 OMNIBUS
N=1,028


**************************************************************************************************************************

This dataset includes cell phone interviews conducted using an RDD sample of cell phone numbers. 
Cell phone interviews include households that are cell-only as well as those that also have a landline phone. 
WEIGHT is the weight for the combined sample of all landline and cell phone interviews and should be used for analysis of this data.
 
***************************************************************************************************************************

Geographic data for landline records are derived from their telephone exchange; for cell phone records, the information is derived from self-reported zip code.
We do this because the error rate in the original geographic information associated with the sample is quite high, especially for respondents from the cell phone sample. 

For respondents who do not provide a zip code or for those we cannot match, we use the sample geographic information. 

To protect the privacy of respondents, telephone numbers, county of residence and zip code have been removed from the public data file.

***************************************************************************************************************************

In this and other weekly omnibus surveys beginning in May 2010, demographic variable names match those in the questionnaire, but the order of demographics in the survey may vary.

***************************************************************************************************************************

Releases from this survey:

June 28, 2017, "Public Supports Aim of Making It 'Easy' for All Citizens to Vote"
http://www.people-press.org/2017/06/28/public-supports-aim-of-making-it-easy-for-all-citizens-to-vote/

***************************************************************************************************************************
SYNTAX

**The race-ethnicity variable (racethn) was computed using the following syntax**
recode race (3 thru 5=3)(6 thru 10=4)(99=9)(else=copy) into racethn.
val label racethn 1'White non-Hispanic' 2 'Black non-Hispanic' 3'Hispanic' 4'Other' 9'Don’t know/Refused (VOL.)'.
var label racethn 'Race-Ethnicity'.

**The income summary variable (income) was computed using the following syntax**
**Pew Research Center reports use income variable, not inc variable; however, Z-9a is asked as to probe respondents who did not want to answer income (see questionnaire for details)**
recode income (1 thru 3=1)(4,5=2)(6=3)(7,8,11 thru 15=4)(9=96)(10=97)(98,99=99) into inc.
val lab inc 1 'Under $30K' 2 '$30K-LT $50K' 3 '$50K-LT $75K' 4 '$75K+'96'Under $50K (unspecified)' 97'$50K-LT $100K (unspecified)'99 'Undesignated'.
var lab inc 'Income summary'.
