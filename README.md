# RetirementCaseStudy1
A case study for retirement as government employee. Try out the [Shiny app](https://amritpatel.shinyapps.io/RetirementCaseStudy2/)!

###Key Assumptions:
- Start work at 23
- Retire at 57
- 5% TSP contribution
- 5% inflation adjusted returns
- 2.5% inflation rate
- 2016 Dollars
- Income, Tax Brackets, Social Security are inflation adjusted
- Pay progression in pay.csv

####3/3/2016 Modifications
- Implemented a spend column based on a savings rate
- Retirement spending based on a percent of pre-retirement spending
- Added acct_opt function which tries to optimize account withdrawals including implementing a Roth Conversion Ladder while trying to maintain a constant level of spending
- Implemented the acct_opt function in the retirement loop
- Now draws down retirement accounts to zero while maintaining constant-ish level of spending

####2/27/2016 6:38pm Modifications
- Added FAFSA (Federal Methodology) Expected Family Contribution  

####2/27/2016 12:16pm Modifications
- Used pre-allocated vectors for retirement instead of appending to vectors
- Incorporated availRoth bug fix

####2/26/2016 Modifications
Modified Working data:
- Added FERS reduction

Modified Retirement Loop w/ following assumptions:
 
- Call pension function
- Use r_ for tax inputs
- All vectors are pre-allocated
- Turned off PDF printing (significantly increases runtime)
- Retirement age is now a variable
- Add logic accounting for "early" retirement which makes pre-63 withdrawals @?% from TSP based on Roth ladder concept; the % is a variable
- Begin drawing pension at 57
- ~~SS + pension is still re-invested into TSP~~
- Added age-dependent pension and SS vector initializations
- Assumes variable TSP withdrawal rate after 63
- Taxable investment account is factored in with variable time-independent withdrawal rate
- HSA balance is not factored in
- Roth IRA balance is factored in with variable time-independent withdrawal rate
- Traditional IRA is not factored in 
- Mortgage balance not factored in (optimize taxable balance to payoff mortgage once retire?)
- Kid(s) going to college not factored in
- Spending vs. time not factored in
- Available account balance column added
