A case study for retirement as government employee and working spouse

### Key Assumptions:
+ Start work at 24
+ Retire at 37
+ 5% TSP contribution
+ 5% inflation adjusted returns
+ 2015 Dollars
+ Income, Tax Brackets, Social Security are inflation adjusted
+ Time-dependent inputs in pay.csv
+ Other inputs in input.R

### Mods in this patch:
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
- Married filing jointly (assumes two income streams; one teacher + one GG14 NRC emp)
- Maxing TSP and 403(b) beginning in 8th year of career (~5% TSP matching before)
- Maxing HSA in 8th year of career
- Maxing Roth IRA in 8th year of career
- Deposit of $25k in taxable investment account in 8th year of career
- Could pay mortgage off at retirement by using ~$50k in taxable investment account
- Roth IRA to help pay for college if necessary (~$187k available -- i.e. contributions only)
