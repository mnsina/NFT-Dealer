library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(bslib)
library(thematic)
library(shinyauthr)
library(rdrop2)
library(data.table)
library(DT)
library(httr)
library(rjson)
library(magick)
library(shinyalert)
library(waiter)


#0) Custom warnings/errors

options(shiny.error = function() {
  stop("Please wait a few seconds...")
})


#1) Tabs

   Tab_Login <- tabPanel(
   title = icon("lock"), 
   value = "login", 
   shinyauthr::loginUI("login")
   )

  
   Tab_Company_1 <- tabPanel("Information", 
                      
  HTML("<h2 id='Who we are'>Who We Are:</h2>
  <p>We are an ERC-721 smart contract that buys and sells NFT tokens on the ethereum goerli network.</p>
  <h2 id='enterprise-characteristics-'>Enterprise Characteristics:</h2>
  <p>1) Stocks: There are 100 stocks without voting rights, each has a unit price of 0.01 ethers. </p>
  <p>2) Founder: Can change the CFO, CEO and end the enterprise. </p>
  <p>3) CEO: Accepts or rejects clients offers and decides the price at which the NFT&#39;s bought by the firm will be sold. </p>
  <p>4) CFO: Decides the amount and when dividends are pay to shareholders. </p>
  <p>5) Enterprise Bids: All enterprise bids are linked to a previous client offer.</p>
  <p>6) Enterprise Offers: All the NFT&#39;s that the enterprise offers were bought previously to a client.</p>
  <p>7) Clients Bid: All client bids are linked to a previous enterprise offer. </p>
  <p>8) Clients Offers: All client offers are reviewed by the CEO before being accepted or rejected. </p>
  <h2 id='enterprise-etherscan-link-'>Enterprise Etherscan Link:</h2>"),
  
  actionButton("B_Etherscan1", "Go to Etherscan", class = "btn-success", style = "color: white;", 
               onclick ="window.open('https://goerli.etherscan.io/address/0x56305dd84673249753cfd7be4b1e070000e0cd25', '_blank')"),
  
  HTML("<h2 id='enterprise-github-link-'>Enterprise Github Link:</h2>"),
  actionButton("B_Github", "Go to Github", class = "btn-success", style = "color: white;", 
               onclick ="window.open('https://github.com/mnsina/NFT-Dealer', '_blank')"),
  HTML("<br> &nbsp"),
                      
                      useShinyjs()
  )
  
  
  Tab_Company_2 <- tabPanel("Instructions", useWaiter(), 
                      
  HTML("<h2 id='use-this-dapp-'>Use this Dapp:</h2>
  <p>1) Download and create an account on Metamask:</p>"),
  actionButton("B_Metamask1", "Download Metamask", class = "btn-success", style = "color: white;", 
               onclick ="window.open('https://metamask.io/download/', '_blank')"),
  HTML("<br> &nbsp"),
  HTML("<p>2) Connect to your Metamask wallet on the goerli network:</p>"),
  actionButton("B_Metamask2", "Connect to Metamask", class = "btn-success", style = "color: white;"),
  
  
  HTML("<h2 id='become-a-Shareholder-'>Become a Shareholder:</h2>"),
  HTML("<p>1) Confirm that there are available shares to buy at 0.01 ethers: </p>"),
  
  actionButton("B-Shares", "Available Shares:"),
  textOutput("Available_Stocks", container = pre),
  
  HTML("<p>2) Confirm that there are enough funds in your wallet to become a shareholder: </p>"),
  actionButton("B-ClientFunds", "Wallet Funds (wei):"),
  textOutput("Cash_Personal", container = pre),
  
  actionButton("B-BuyShare", "Buy Share", class = "btn-warning", style = "color: white;"),
  HTML("<br> &nbsp"),
  
  HTML("<h2 id='sell-an-nft-'>Sell an NFT:</h2>
  <p>1) Please complete the following form to submit an offer:</p>"),
  wellPanel(
  textInput("Client-Offer1", label = "NFT smart contract address:", value = ""),
  textInput("Client-Offer2", label = "NFT id:", value = ""),
  textInput("Client-Offer3", label = "NFT Price (wei):", value = ""),
  actionButton("B-ClientOffer", "Submit Offer", class = "btn-warning", style = "color: white;")),
  HTML("<br> &nbsp"),
  
  HTML("<p>2) Check that the CEO has reviewed the offer using the function &quot;NFT_Clients_Offers&quot;.  </p>"),
  actionButton("B-ClientOffer2", "Refresh Clients Offers"),
  wellPanel(
  DTOutput("table1")
  ),
  HTML("<br> &nbsp"),
  HTML(" <p>3) Use the function &quot;NFT_Enterprise_Bids&quot; to search the enterprise's bid linked to your offer.</p>"),
  actionButton("B-EnterpriseBid1", "Refresh Enterprise Bids"),
  wellPanel(
  DTOutput("table2")
  ),
  HTML("<br> &nbsp"),
  
  HTML("<p>4) Approve control of the NFT token to the enterprise address (NFT Original Contract) and then use the function &quot;NFT_Client_Sell&quot; with the bid id that the CEO wrote on the previous step. </p>"),
  wellPanel(
  textInput("Enterprise-Bid1", label = "NFT smart contract address:", value = ""),
  textInput("Enterprise-Bid2", label = "NFT id:", value = ""),
  textInput("Enterprise-Bid3", label = "Enterprise bid id:", value = ""),
  actionButton("B-ApproveControl", "Approve Control", class = "btn-warning", style = "color: white;"),
  actionButton("B-ClientSell", "Sell NFT", class = "btn-warning", style = "color: white;")),
  HTML("<br> &nbsp"),
  
  HTML("<h2 id='buy-an-nft-'>Buy an NFT:</h2>
  <p>1) Use the function &quot;NFT_Enterprise_Offers&quot; to search active offers.</p>"),
  actionButton("B-EnterpriseOffers", "Refresh Enterprise Offers"),
  wellPanel(
  DTOutput("table3")
  ),
  HTML("<br> &nbsp"),
  
  HTML("<p>2) Use the function &quot;NFT_Client_Buy&quot; using the information of the previous step.</p>"),
  wellPanel(
  textInput("Client-Buy1", label = "NFT smart contract address:", value = ""),
  textInput("Client-Buy2", label = "NFT id:", value = ""),
  textInput("Client-Buy3", label = "Enterprise offer id:", value = ""),
  textInput("Client-Buy4", label = "Price (wei):", value = ""),
  actionButton("B-ClientBuy", "Buy NFT", class = "btn-warning", style = "color: white;")),
  
  
  HTML("<br> &nbsp"),                    
  useShinyjs()
  )
  
  
   Tab_Company_3 <- tabPanel("Addresses", HTML("<br> The main addresses are listed below:
                                        <br> <br>"),
                      
                      useShinyjs(),
                      #tags$script(src="Metamask.js"),
                      
                      actionButton("B-Meta", "Investor Address:"),
                      textOutput("Metamask", container = pre),
                      
                      actionButton("B-Enterprise", "Enterprise Address:"),
                      textOutput("Enterprise", container = pre),
                      
                      actionButton("B-CEO", "CEO Address:"),
                      textOutput("CEO", container = pre),
                      
                      actionButton("B-CFO", "CFO Address:"),
                      textOutput("CFO", container = pre),
                      
                      actionButton("B-Founder", "Founder Address:"),
                      textOutput("Founder", container = pre)#,
                      
                      #tags$script(src ="web3.js"),
                      #tags$script(src ="require.js") 
                      
  )
  
  
  Tab_Register <-   tabPanel(value="Register", title = icon("user"), wellPanel(
                    tags$h2("Please register", class = "text-center", style = "padding-top: 0;"),
                      
                      useShinyjs(), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                      
                      textInput("User1", tagList(shiny::icon("user"), "Enter Email"), value = ""),
                      
                      textInput("Pass1", tagList(shiny::icon("unlock-alt"), "Enter password"), value = ""),
                      
                      textInput("Pass2", tagList(shiny::icon("unlock-alt"), "Repeat password"), value = ""),
                      
                      div(style = "text-align: center;",
                      actionButton("Btn1", "Register", class = "btn-primary", style = "color: white;"))
                      
  ))
  
  
  
  Tab_Investors_1 <- tabPanel("Shareholders", HTML("<br> All shareholders addresses are listed below: <br> <br>"),
                              
                     actionButton("B-Shareholders1", "Shareholders Addresses:"),
                     tabPanel("", title = icon("user"), wellPanel(
                     tags$h2("", class = "text-center", style = "padding-top: 0;"),
                     DTOutput("table4")
                     )),
                     HTML("<br> &nbsp"))
  
  Tab_Investors_2 <- tabPanel("Financial Results", HTML("<br> All financial transactions are listed below: <br> <br>"),
                      
                      actionButton("B-Financial1", "Financial Transactions:"),
                      tabPanel("", title = icon("user"), wellPanel(
                      tags$h2("", class = "text-center", style = "padding-top: 0;"),
                      DTOutput("table5")
                      )),
                      HTML("<br> &nbsp"))
  
  Tab_Investors_3 <- tabPanel("C-Levels", HTML("<h2 id='CEO Functions-'>CEO Functions:</h2>
             <p>1) Review a client specific offer using the function &quot;NFT_CEO_Offer_Review&quot;:</p>"),
             tabPanel("", title = icon("user"), wellPanel(
             tags$h2("", class = "text-center", style = "padding-top: 0;"),
             textInput("CEO-Review-Offer1", label = "Offer id:", value = ""),
             textInput("CEO-Review-Offer2", label = "Comments:", value = ""),
             actionButton("B-ReviewReject", "Reject Offer", class = "btn-warning", style = "color: white;"),
             actionButton("B-ReviewAccept", "Accept Offer", class = "btn-warning", style = "color: white;"))),
             HTML("<br> &nbsp"),
             
             HTML("<p>2) Create a new offer using the function &quot;NFT_CEO_Offer_New&quot;:</p>"),
             tabPanel("", title = icon("user"), wellPanel(
             tags$h2("", class = "text-center", style = "padding-top: 0;"),
             textInput("CEO-New-Offer1", label = "NFT's smart contract address:", value = ""),
             textInput("CEO-New-Offer2", label = "NFT's id:", value = ""),
             textInput("CEO-New-Offer3", label = "NFT´s price (wei):", value = ""),
             actionButton("B-OfferCEO", "Create Offer", class = "btn-warning", style = "color: white;"))),
             HTML("<br> &nbsp"),
             
             HTML("<h2 id='CFO Functions-'>CFO Functions:</h2>
             <p>1) Pay dividends to shareholders using the function &quot;Dividends_Pay&quot;:</p>"),
             tabPanel("", title = icon("user"), wellPanel(
             tags$h2("", class = "text-center", style = "padding-top: 0;"),
             textInput("CFO-Dividends", label = "Amount for each shareholder (wei):", value = ""),
             actionButton("B-DividendsCFO", "Pay Dividends", class = "btn-warning", style = "color: white;"))),
             HTML("<br> &nbsp"),
             
             HTML("<h2 id='Founder Functions-'>Founder Functions:</h2>
             <p>1) Change the address of the active CEO using the function &quot;Change_CEO&quot;:</p>"),
             tabPanel("", title = icon("user"), wellPanel(
             tags$h2("", class = "text-center", style = "padding-top: 0;"),
             textInput("Founder-CEO", label = "New CEO address:", value = ""),
             actionButton("B-FounderCEO", "Change CEO", class = "btn-warning", style = "color: white;"))),
             HTML("<br> &nbsp"),
             HTML("<p>2) Change the address of the active CFO using the function &quot;Change_CFO&quot;:</p>"),
             tabPanel("", title = icon("user"), wellPanel(
             tags$h2("", class = "text-center", style = "padding-top: 0;"),
             textInput("Founder-CFO", label = "New CFO address:", value = ""),
             actionButton("B-FounderCFO", "Change CFO", class = "btn-warning", style = "color: white;"))),
             HTML("<br> &nbsp"),
             HTML("<p>3) End the enterprise using the function &quot;End_Enterprise&quot;:</p>"),
             tabPanel("", title = icon("user"), wellPanel(
             tags$h2("", class = "text-center", style = "padding-top: 0;"),
             actionButton("B-FounderEnd", "End Enterprise", class = "btn-warning", style = "color: white;"))),
             HTML("<br> &nbsp")
             
             )
  
  
  
#2) Tab Panels
  
  Tabs_Company <- tabPanel("Company", fluidPage(tabsetPanel(
    Tab_Company_1,
    Tab_Company_2,
    Tab_Company_3,
    tags$script(src ="web3.js")
  )))
  
  Tabs_Investors <- tabPanel("Investors", fluidPage(tabsetPanel(
                  Tab_Investors_1,
                  Tab_Investors_2,
                  Tab_Investors_3,
                  tags$script(src ="web3.js")
  )))
  

  
  
#3) App Layout

  navbarPage("The Crypto Corp.", collapsible = TRUE, inverse = TRUE, theme = bs_theme(bg = "#005678", 
                                 fg = "#D1F7FF", primary = "#05D9E8", base_font = font_google("Prompt"),
                                 code_font = font_google("JetBrains Mono")), id="tabs",
          Tab_Login,
          Tabs_Company,
          Tabs_Investors,
          Tab_Register,
          tags$head(tags$style(".shiny-output-error{color: white;}")),
          tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
          tags$head(tags$style(".shiny-output-error:after{content: 'Loading... Please wait...';
visibility: visible}"))
)

