# RetirementCaseStudy1
A case study for retirement as government employee 

###Key Assumptions:
+ Start work at 23
+ Retire at 57
+ 5% TSP contribution
+ 5% inflation adjusted returns
+ 2.5% inflation rate
+ 2016 Dollars
+ Income, Tax Brackets, Social Security are inflation adjusted
+ Pay progression in pay.csv

####2/27/2016 12:16pm Modifications
+ Used pre-allocated vectors for retirement instead of appending to vectors
+ Incorporated availRoth bug fix

####2/26/2016 Modifications
Modified Working data:
- Added FERS reduction

Modified Retirement Loop w/ following assumptions:
 
- Call pension function
- Use r_ for tax inputs
- All vectors are pre-allocated
- Turned off PDF printing (significantly increases runtime)
- Retirement age is now a variable
- Add logic accounting for "early" retirement which makes withdrawals @?% from TSP based on Roth ladder concept; the % is a variable
- Begin drawing pension at 57; none before
- SS + pension is still re-invested into TSP
- Added age-dependent pension and SS vector initializations
- Assumes variable TSP withdrawal rate after 63
- Taxable investment account is factored in
- HSA balance is not factored in
- Roth IRA balance is factored in
- Traditional IRA is not factored in
- Mortgage balance not factored in (optimize taxable balance to payoff mortgage once retire?)
- Kid(s) going to college not factored in
- Spending vs. time not factored in
- Available account balance column added
- Married fi
