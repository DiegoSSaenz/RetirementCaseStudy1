shinyUI(
  navbarPage("",
             tabPanel("Main",
                      pageWithSidebar(
                        # Application title
                        headerPanel(a(href="https://github.com/DiegoSSaenz/RetirementCaseStudy1", "finPlan"),
                                    windowTitle="finPlan"),
                        
                        sidebarPanel(
                          p("A case study for retirement as government employee."),
                          # a(href="http://www.google.com", "stuff"),
                          # " and more stuff."),
                          br(),
                          sliderInput(inputId="retireAge", label = "Choose your retirement age:", 
                                      min=35,
                                      max=75,
                                      value=46
                          ),
                          sliderInput(inputId="ladder", label = "Roth ladder withdrawal %:", 
                                      min=0,
                                      max=50,
                                      value=9
                          ),
                          sliderInput(inputId="tspRate", label = "Post-62 TSP withdrawal %:", 
                                      min=0,
                                      max=50,
                                      value=4
                          ),
                          sliderInput(inputId="rothRate", label = "Post-retirement Roth IRA withdrawal %:", 
                                      min=0,
                                      max=50,
                                      value=4
                          ),
                          sliderInput(inputId="taxAcctRate", label = "Pre-63 taxable account withdrawal %:", 
                                      min=0,
                                      max=50,
                                      value=0
                          ),
                          sliderInput(inputId="ret", label = "Average investment return %:", 
                                      min=0,
                                      max=20,
                                      value=5
                          ),
                          submitButton('Submit')
                          # width = 8
                        ),
                        
                        mainPanel(
                          imageOutput("img1")
                          # width = 8
                        )  
                      )
             ),
            tabPanel("Input Load",
                    pageWithSidebar(
                    # Application title
                    headerPanel(a(href="https://github.com/DiegoSSaenz/RetirementCaseStudy1", "finPlan"),
                                 windowTitle="finPlan"),
           
                    sidebarPanel(
                        p("A case study for retirement as government employee."),
                        # a(href="http://www.google.com", "stuff"),
                        # " and more stuff."),
                        br(),
                        fileInput('file1', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')),
                        tags$hr(),
                        checkboxInput('header', 'Header', TRUE),
                        radioButtons('sep', 'Separator',
                                     c(Comma=',',
                                       Semicolon=';',
                                       Tab='\t'),
                                     ','),
                        radioButtons('quote', 'Quote',
                                     c(None='',
                                       'Double Quote'='"',
                                       'Single Quote'="'"),
                                     '"')
                    ),
           
                    mainPanel(
                        tableOutput('contents')
                        # width = 8
                    )  
                    )
              )
  )
)