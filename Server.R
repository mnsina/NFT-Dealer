library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(bslib)
library(shinyauthr)
library(rdrop2)
library(data.table)
library(DT)
library(httr)
library(rjson)
library(magick)
library(shinyalert)
library(waiter)


ApiKeyEtherscan<-"QFU3USEYBT19XUWMJX6K5N9HEI5HUQ4IHB"

ContractAddress<-"0x56305dd84673249753cFd7bE4B1e070000E0Cd25"


function(input, output, session){
  
  #1) Load dropbox token
  
  drop_auth(rdstoken = "token.rds")
  
  
  #2) Create datatable of dropbox users
  
  users_db<-reactive({
    
    users_db<-drop_dir(path = "/USERS_DB")
    users_db<-data.table(users_db$name)
    setnames(users_db, old="V1", new = "name", skip_absent=TRUE)
    users_db<-users_db[, name:=gsub(pattern = ".csv", replacement = "", name)]
    users_db<-users_db[, Users:=substr(name, 7, nchar(name))]
    pass0<-data.table(as.numeric(gregexpr(pattern = " - Pass:", users_db$Users)))
    setnames(pass0, old="V1", new = "pass0", skip_absent=TRUE)
    users_db<-cbind(users_db, pass0)
    users_db<-users_db[, Users:=substr(Users, 1, pass0-1)]
    users_db<-users_db[, Users:=tolower(gsub(pattern = " ", replacement = "", Users))]
    users_db<-users_db[, Passwords:=substr(name, pass0+6+9-1, nchar(name))]
    users_db<-users_db[, Passwords:=gsub(pattern = " ", replacement = "", Passwords)]
    users_db<-users_db[, pass0:=NULL]
    users_db<-users_db[, name:=NULL]
    
  })
  
  
  output$tableUsers<-renderDT({
    
    users_db()
    
  }, filter="top")
  
  output$download0 <- downloadHandler(
  filename = function() {
  paste("Files", ".csv", sep="")
  },
  content = function(file) {
  write.csv(users_db(), file)
  }
  )
  
  
  #3) Register a new user
  
  observeEvent(input$Btn1, if(input$Pass1!=input$Pass2) {
    
  shinyjs::alert("Passwords don`t match") } else {
    
  if(input$User1!=tolower(input$User1))  {shinyjs::alert("Please use only lowercase letters and digits")} else{
      
  if(tolower(input$User1) %in% users_db2$user) { shinyjs::alert("User already exists") } 
      
  else { if(grepl("@", input$User1, fixed=TRUE)==FALSE ) { 
  shinyjs::alert("Please enter an email address") } else {
          
  {new_user<-data.table(user=input$User1, password=input$Pass1, permissions="standard", name=input$User1)
          
   fwrite(new_user, paste0("User: ", tolower(input$User1), " - Pass: ", input$Pass1, ".csv"))
          
   drop_upload(paste0("User: ", input$User1, " - Pass: ", input$Pass1, ".csv"), path = "/USERS_DB")
          
    shinyjs::alert("The user was successfully created")
    session$reload()
    
    }}
    }}}
    )
  
  
  #4) Login to app
  
  users_db2<-drop_dir(path = "/USERS_DB")
  users_db2<-data.table(users_db2$name)
  setnames(users_db2, old="V1", new = "name", skip_absent=TRUE)
  users_db2<-users_db2[, name:=gsub(pattern = ".csv", replacement = "", name)]
  users_db2<-users_db2[, user:=substr(name, 7, nchar(name))]
  pass1<-data.table(as.numeric(gregexpr(pattern = " - Pass:", users_db2$user)))
  setnames(pass1, old="V1", new = "pass1", skip_absent=TRUE)
  users_db2<-cbind(users_db2, pass1)
  users_db2<-users_db2[, user:=substr(user, 1, pass1-1)]
  users_db2<-users_db2[, user:=tolower(gsub(pattern = " ", replacement = "", user))]
  users_db2<-users_db2[, password:=substr(name, pass1+6+9-1, nchar(name))]
  users_db2<-users_db2[, password:=gsub(pattern = " ", replacement = "", password)]
  users_db2<-users_db2[, pass1:=NULL]
  users_db2<-users_db2[, name:=NULL]
  
  
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(class="nav navbar-nav navbar-right", tags$li(div(
    style = "padding: 10px; padding-top: 8px; padding-bottom: 0;", shinyauthr::logoutUI("logout")
    ))
    ))
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    reactive(credentials()$user_auth)
  )
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = users_db2,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init()),
    reload_on_logout = TRUE
  )
  
  
  observe({
    if (credentials()$user_auth) {
      hideTab("tabs", "login")
      hideTab("tabs", "Register")
      showTab("tabs", "Company")
      showTab("tabs", "Investors")
      showTab("tabs", "Financial Results")
      showTab("tabs", "C-Levels")
      admin<-credentials()$info$user
      if (admin=="mnsina@uc.cl"){
        showTab("tabs2", "Users")}
    } 
    else {
      hideTab("tabs", "Login")
      hideTab("tabs", "Company")
      hideTab("tabs", "Investors")
      hideTab("tabs", "Financial Results")
      hideTab("tabs", "C-Levels")
    }
  })
  
  
  
  #5.0 Read Contract Methods
  
    #5.1 Get Enterprise Address
  
   onclick("B-Enterprise", {
    
    runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
          
    async function prueba(){
          
    const myval = await contract1.methods.Enterprise().call();
          
    console.log(myval);
          
    Shiny.setInputValue('Enterprise',  myval);
          
    }
          
    prueba();"
    )
    
    output$Enterprise<-renderText({input$Enterprise})
    
    })
  
  
    #5.2 Get Investor Address
  
   onclick("B-Meta", {
   
    runjs("if (typeof window.ethereum !== 'undefined') {
    window.alert('MetaMask is installed!');
    }")
    
    runjs("async function onInit() {
    await window.ethereum.enable();
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
    //window.alert(account)
    window.ethereum.on('accountsChanged', function (accounts) {
    // Time to reload your interface with accounts[0]!
    window.alert(accounts[0])
    });
    Shiny.setInputValue('CuentaMetamask', account);
    }

    onInit();
         "
          
          )
    
    print(input$CuentaMetamask)
    
    output$Metamask<-renderText({input$CuentaMetamask
    })
    
  })
   
   
   #5.3 Get CEO Address
   
   onclick("B-CEO", {
     
     runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
          
    async function prueba(){
          
    const myval = await contract1.methods.CEO().call();
          
    console.log(myval);
          
    Shiny.setInputValue('CEO',  myval);
          
    }
          
    prueba();"
           
     )
     
     output$CEO<-renderText({input$CEO})
     
   })

   
   #5.4 Get CFO Address
   
   onclick("B-CFO", {
     
     runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
          
    async function prueba(){
          
    const myval = await contract1.methods.CFO().call();
          
    console.log(myval);
          
    Shiny.setInputValue('CFO',  myval);
          
    }
          
    prueba();"
           
     )
     
     output$CFO<-renderText({input$CFO})
     
   })
  
   
   #5.5 Get Founder Address
   
   onclick("B-Founder", {
     
     runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
          
    async function prueba(){
          
    const myval = await contract1.methods.Founder().call();
          
    console.log(myval);
          
    Shiny.setInputValue('Founder',  myval);
          
    }
          
    prueba();"
           
     )
     
     output$Founder<-renderText({input$Founder})
     
   })
   
   
   #5.6 Connect to Metamask
   
   onclick("B_Metamask2", {
     
     runjs("ethereum.request({ method: 'eth_requestAccounts' });")
     
   })
   
   
   #5.7 Get Number of Available Shares
   
   onclick("B-Shares", {
     
     runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
          
    async function prueba(){
          
    const myval = await contract1.methods.Available_Stocks().call();
          
    console.log(myval);
  
    Shiny.setInputValue('Available_Stocks',  myval);
          
    }
          
    prueba();"
           
     )
     
     output$Available_Stocks<-renderText({input$Available_Stocks})
     
   })
   
   
   #5.8 Get Client Wallet Funds
   
   onclick("B-ClientFunds", {
     
     runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
          
    async function prueba(){
    
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
    
    const balance = await web3.eth.getBalance(account); 
  
    Shiny.setInputValue('Cash_Personal',  balance);
          
    }
          
    prueba();"
           
     )
     
     output$Cash_Personal<-renderText({input$Cash_Personal})
     
   })
   
   
   #5.9 Buy 1 Enterprise Share
   
   onclick("B-BuyShare", {
     
     runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.Buy_Share().send({ from: account, value:10000000000000000 })};
    
    prueba();"
           
     )
     
   })
    
   
   #5.10 Client Offer NFT
   
   onclick("B-ClientOffer", {
     
     runjs("
    
    const ClientOffer1 = document.getElementById('Client-Offer1').value; 
    const ClientOffer2 = document.getElementById('Client-Offer2').value; 
    const ClientOffer3 = document.getElementById('Client-Offer3').value; 
    console.log(ClientOffer1);
    console.log(ClientOffer2);
    console.log(ClientOffer3);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.NFT_Client_Offer_New(ClientOffer1, ClientOffer2, ClientOffer3).send({ from: account, value:0 })};
    
    prueba();"
       
           
     )
     
   })
   
   
   #5.11 List All Clients Offers
   
    wTable1 <- Waiter$new(id = "table1", color = transparent(.5), html = spin_loaders(15))
   
    observeEvent(input$"B-ClientOffer2",{
     
     wTable1$show()
     
     Transactions<-content(GET(url = "https://api-goerli.etherscan.io/api", 
                           query = list(module="account", action="txlist", 
                           address=ContractAddress, apikey=ApiKeyEtherscan)))
     
     Transactions<-rbindlist(lapply(Transactions$result, as.data.frame.list))
     Transactions<-Transactions[txreceipt_status==1]
     
     NumberOfTransactions<-length(Transactions$hash)
     
     for( i in 1:NumberOfTransactions){
       
     Hash_Id<-content(GET(url="https://api-goerli.etherscan.io/api", query=list(module="proxy", 
                      action="eth_getTransactionByHash",txhash=Transactions$hash[i],  apiKey=ApiKeyEtherscan)))
     
     if(i==1){
       Hash_Ids<-Hash_Id$result
     }
     
     else {
       Hash_Ids<-rbind(Hash_Ids, Hash_Id$result)
     }
     Sys.sleep(0.20)
     }
     
     Methods_Id<-data.table(Hash_Ids)
     Methods_Id<-Methods_Id[, 9]
     Methods_Id<-substr(Methods_Id$input, 1, 10)
     Methods_Id<-data.table(Methods_Id)
     
     C_NumberOffers<-Methods_Id[Methods_Id=="0x8a9c2689"]
     C_NumberOffers<-as.numeric(C_NumberOffers[, Methods_Id:=.I][.N, 1])
     
     
     runjs(paste0("
     
    const web3 = new Web3(Web3.givenProvider);
    
    const a=[];
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
    
    for (let i = 0; i <", C_NumberOffers,"; i++) {
    
    a[i] = await contract1.methods.NFT_Clients_Offers(i).call(); 
    
    }
          
          Shiny.setInputValue('Clients_Offers',  a);
          Shiny.setInputValue('Clients_Offers_Id',  a.length-1);
    }
    
    console.log(a);
    prueba();
           
    "))

   
     output$table1<-DT::renderDT({
       
       a<-rbindlist(list(as.list(input$Clients_Offers)))
       
       a1<-7
       a2<-11
       aa<-data.table()
       
       for(i in 0:input$Clients_Offers_Id){
         
       aa<-rbind(a[, a1:a2],aa)
       a1=a1+12
       a2=a2+12
       
       }
       
       setcolorder(aa[, Offer_Id:=as.integer(.N:1-1)],  c(6,1,2,3,4,5))
       
       #---------------------------------NFT IMAGE-----------------------------------------------------------------
       for(i in 1:nrow(aa)){
         
         NFT_ABI0<-data.table(content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="contract", action="getabi", address=aa[i,NFT], apikey=ApiKeyEtherscan)))$result)
         Sys.sleep(0.20)
         
         if(i==1){
           NFT_ABI<-data.table(content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="contract", action="getabi", address=aa[i,NFT], apikey=ApiKeyEtherscan)))$result)
           Sys.sleep(0.20)
         }
         
         else {
           NFT_ABI<-rbindlist(list(NFT_ABI, NFT_ABI0), fill = TRUE)
         }
           Sys.sleep(0.20)
       }
       
       aa<-cbind(aa, NFT_ABI)
       aa<-setnames(aa, "V1", "NFT_ABI")
       
       for(i in 1:nrow(aa)){
       runjs(paste0("
       const web3 = new Web3(Web3.givenProvider);
       var contract1 = new  web3.eth.Contract(", aa[i,NFT_ABI],", '",aa[i,NFT],"');
       async function prueba(){
       const myval = await contract1.methods.tokenURI(",aa[i,NFT_Id],").call();
       //console.log(myval);
       Shiny.setInputValue('Foto1",i,"',  myval);
       }
       prueba();"))
         
         NFT_URI0<-data.table(eval(parse(text=paste0("input$Foto1",i))))
         
         # JSON Cases:
         
         if( grepl("ipfs://", NFT_URI0[1], fixed = TRUE)==TRUE ){  
         NFT_JSON0<-paste0('https://', substr(NFT_URI0[1], 0, unlist(gregexpr('://', NFT_URI0[1]))[1]-1),
                           '.io/', substr(NFT_URI0[1], 0, unlist(gregexpr('://', NFT_URI0[1]))[1]-1), '/',
                           substr(NFT_URI0[1],  unlist(gregexpr('://', NFT_URI0[1]))[1]+3, nchar(NFT_URI0[1])))
         NFT_JSON0<-data.table(content(GET(url = NFT_JSON0[1]))$image) 
         } 
           
         else{ if( grepl("https://", NFT_URI0[1], fixed = TRUE)==TRUE ){
         NFT_JSON0<-NFT_URI0[1]
         NFT_JSON0<-data.table(content(GET(url = NFT_JSON0[1]))$image) 
         } 
         else{NFT_JSON0<-data.table("File not found")}
         }
         
         if( grepl("ipfs://", NFT_JSON0[1], fixed = TRUE)==TRUE ){
         NFT_JSON0<-NFT_JSON0[,V1:=paste0('https://', substr(V1, 0, unlist(gregexpr('://',V1))[1]-1),
                                              '.io/', substr(V1, 0, unlist(gregexpr('://', V1))[1]-1), '/',
                                              substr(V1, unlist(gregexpr('://', V1))[1]+3, nchar(V1)))]
         }
         
         else{ if( grepl("https://123", NFT_JSON0[1], fixed = TRUE)==TRUE ){
           NFT_JSON0<-NFT_JSON0 } 
           else{NFT_JSON0<-"https://etherscan.io/images/main/nft-placeholder.svg"}
         }
         
         NFT_JSON0<-NFT_JSON0[,V1:=paste0("<img src=", V1," height='52'></img>")]
         
         
         if(i==1) {
         NFT_URI<-NFT_URI0
         NFT_JSON<-NFT_JSON0
         }
         
         else {
           NFT_URI<-rbind(NFT_URI, NFT_URI0)
           NFT_JSON<-rbind(NFT_JSON, NFT_JSON0)
         }
         }
       
       NFT_URI<-setnames(NFT_URI, "V1", "NFT_URI", skip_absent=TRUE)
       NFT_JSON<-setnames(NFT_JSON, "V1", "NFT_JSON", skip_absent=TRUE)


         
       aa<-cbind(aa, NFT_JSON)
       aa[, NFT_ABI:=NULL]
       aa<-setnames(aa, "NFT_JSON", "NFT_Image")
       #---------------------------------NFT IMAGE-----------------------------------------------------------------
       
       
       DT::datatable(aa,  escape = FALSE, filter="top", style = "bootstrap4")
       
       })
     
     wTable1$hide()
       
   })
   
  
   #5.12 List All Enterprise Bids
    
    wTable2 <- Waiter$new(id = "table2", color = transparent(.5), html = spin_loaders(15))

    observeEvent(input$"B-EnterpriseBid1",{
     
     wTable2$show()
     
     Transactions<-content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="account", action="txlist", 
                           address=ContractAddress, apikey=ApiKeyEtherscan)))
     
     Transactions<-rbindlist(lapply(Transactions$result, as.data.frame.list))
     Transactions<-Transactions[txreceipt_status==1]
     
     NumberOfTransactions<-length(Transactions$hash)
     
     for( i in 1:NumberOfTransactions){
       
       Hash_Id<-content(GET(url="https://api-goerli.etherscan.io/api", query=list(module="proxy", 
                        action="eth_getTransactionByHash",txhash=Transactions$hash[i],  apiKey=ApiKeyEtherscan)))
       
       if(i==1){
         Hash_Ids<-Hash_Id$result
       }
       
       else {
         Hash_Ids<-rbind(Hash_Ids, Hash_Id$result)
       }
       Sys.sleep(0.20)
     }
     
     Methods_Id<-data.table(Hash_Ids)
     Methods_Id<-Methods_Id[, 9]
     Methods_Id<-paste0(substr(Methods_Id$input, 1, 10),"-",substr(Methods_Id$input, 138, 138))
     Methods_Id<-data.table(Methods_Id)
     
     E_NumberBids<-Methods_Id[Methods_Id=="0x5b6010c5-1"]
     E_NumberBids<-as.numeric(E_NumberBids[, Methods_Id:=.I][.N, 1])
     
     
     runjs(paste0("
     
    const web3 = new Web3(Web3.givenProvider);
    
    const a=[];
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
          
    async function prueba(){
    
    for (let i = 0; i <", E_NumberBids,"; i++) {
    
    a[i] = await contract1.methods.NFT_Enterprise_Bids(i).call(); 
    
    }
          
          Shiny.setInputValue('Enterprise_Bids',  a);
          Shiny.setInputValue('Enterprise_Bids_Id',  a.length-1);
    }
    
    console.log(a);
    
    prueba();
           
    "))
     
     
     output$table2<-DT::renderDT({
       
       a<-rbindlist(list(as.list(input$Enterprise_Bids)))
       
       a1<-7
       a2<-11
       aa<-data.table()
       
       for(i in 0:input$Enterprise_Bids_Id){
         
         aa<-rbind(a[, a1:a2],aa)
         a1=a1+12
         a2=a2+12
         
       }
       
       setcolorder(aa[, Bid_Id:=as.integer(.N:1-1)],  c(6,1,2,3,4,5))
       
       
       #---------------------------------NFT IMAGE-----------------------------------------------------------------
       for(i in 1:nrow(aa)){
         
         NFT_ABI0<-data.table(content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="contract", action="getabi", address=aa[i,NFT], apikey=ApiKeyEtherscan)))$result)
         Sys.sleep(0.20)
         
         if(i==1){
           NFT_ABI<-data.table(content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="contract", action="getabi", address=aa[i,NFT], apikey=ApiKeyEtherscan)))$result)
           Sys.sleep(0.20)
         }
         
         else {
           NFT_ABI<-rbindlist(list(NFT_ABI, NFT_ABI0), fill = TRUE)
         }
         Sys.sleep(0.20)
       }
       
       aa<-cbind(aa, NFT_ABI)
       aa<-setnames(aa, "V1", "NFT_ABI")
       
       for(i in 1:nrow(aa)){
         runjs(paste0("
       const web3 = new Web3(Web3.givenProvider);
       var contract1 = new  web3.eth.Contract(", aa[i,NFT_ABI],", '",aa[i,NFT],"');
       async function prueba(){
       const myval = await contract1.methods.tokenURI(",aa[i,NFT_Id],").call();
       //console.log(myval);
       Shiny.setInputValue('Foto2",i,"',  myval);
       }
       prueba();"))
         
         NFT_URI0<-data.table(eval(parse(text=paste0("input$Foto2",i))))
         
         # JSON Cases:
         
         if( grepl("ipfs://", NFT_URI0[1], fixed = TRUE)==TRUE ){  
           NFT_JSON0<-paste0('https://', substr(NFT_URI0[1], 0, unlist(gregexpr('://', NFT_URI0[1]))[1]-1),
                             '.io/', substr(NFT_URI0[1], 0, unlist(gregexpr('://', NFT_URI0[1]))[1]-1), '/',
                             substr(NFT_URI0[1],  unlist(gregexpr('://', NFT_URI0[1]))[1]+3, nchar(NFT_URI0[1])))
           NFT_JSON0<-data.table(content(GET(url = NFT_JSON0[1]))$image) 
         } 
         
         else{ if( grepl("https://", NFT_URI0[1], fixed = TRUE)==TRUE ){
           NFT_JSON0<-NFT_URI0[1]
           NFT_JSON0<-data.table(content(GET(url = NFT_JSON0[1]))$image) 
         } 
           else{NFT_JSON0<-data.table("File not found")}
         }
         
         if( grepl("ipfs://", NFT_JSON0[1], fixed = TRUE)==TRUE ){
           NFT_JSON0<-NFT_JSON0[,V1:=paste0('https://', substr(V1, 0, unlist(gregexpr('://',V1))[1]-1),
                                            '.io/', substr(V1, 0, unlist(gregexpr('://', V1))[1]-1), '/',
                                            substr(V1, unlist(gregexpr('://', V1))[1]+3, nchar(V1)))]
         }
         
         else{ if( grepl("https://123", NFT_JSON0[1], fixed = TRUE)==TRUE ){
           NFT_JSON0<-NFT_JSON0 } 
           else{NFT_JSON0<-"https://etherscan.io/images/main/nft-placeholder.svg"}
         }
         
         NFT_JSON0<-NFT_JSON0[,V1:=paste0("<img src=", V1," height='52'></img>")]
         
         
         if(i==1) {
           NFT_URI<-NFT_URI0
           NFT_JSON<-NFT_JSON0
         }
         
         else {
           NFT_URI<-rbind(NFT_URI, NFT_URI0)
           NFT_JSON<-rbind(NFT_JSON, NFT_JSON0)
         }
       }
       
       NFT_URI<-setnames(NFT_URI, "V1", "NFT_URI", skip_absent=TRUE)
       NFT_JSON<-setnames(NFT_JSON, "V1", "NFT_JSON", skip_absent=TRUE)
       
       
       
       aa<-cbind(aa, NFT_JSON)
       aa[, NFT_ABI:=NULL]
       aa<-setnames(aa, "NFT_JSON", "NFT_Image")
       #---------------------------------NFT IMAGE-----------------------------------------------------------------
       
       
       DT::datatable(aa,  escape = FALSE, filter="top", style = "bootstrap4")
       
     })
     
     wTable2$hide()
     
   })      
   
   
   #5.13 CEO Review Clients Offers
   
   onclick("B-ReviewReject", {
     
     runjs("
    
    const ReviewOffer1 = document.getElementById('CEO-Review-Offer1').value; 
    const ReviewOffer2 = document.getElementById('CEO-Review-Offer2').value; 
    console.log(ReviewOffer1);
    console.log(ReviewOffer2);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.NFT_CEO_Offer_Review(ReviewOffer1, false, ReviewOffer2).send({ from: account, value:0 })};
    
    prueba();"
           
           
     )
     
   })
   
   
   onclick("B-ReviewAccept", {
     
     runjs("
    
    const ReviewOffer1 = document.getElementById('CEO-Review-Offer1').value; 
    const ReviewOffer2 = document.getElementById('CEO-Review-Offer2').value; 
    console.log(ReviewOffer1);
    console.log(ReviewOffer2);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.NFT_CEO_Offer_Review(ReviewOffer1, true, ReviewOffer2).send({ from: account, value:0 })};
    
    prueba();"
           
           
     )
     
   })
   
   
   #5.14 Client: Sell an NFT
   
   onclick("B-ApproveControl", {
     
     ABI<-content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="contract", action="getabi", 
                           address=ContractAddress, apikey=ApiKeyEtherscan)))
     ABI<-ABI$result
     
     runjs(paste0("
    
    const Address_NFT = document.getElementById('Enterprise-Bid1').value; 
    const Id_NFT = document.getElementById('Enterprise-Bid2').value; 
    console.log(Address_NFT);
    console.log(Id_NFT);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract(", ABI,", Address_NFT);
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.approve('0x56305dd84673249753cFd7bE4B1e070000E0Cd25', Id_NFT).send({ from: account, value:0 })};
    
    prueba();"
           
           
     ))
     
   })
   
   onclick("B-ClientSell", {
     
     runjs("
    
    const Address_NFT = document.getElementById('Enterprise-Bid1').value; 
    const Id_NFT = document.getElementById('Enterprise-Bid2').value; 
    const Id_Bid = document.getElementById('Enterprise-Bid3').value;
    
    console.log(Address_NFT);
    console.log(Id_NFT);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.NFT_Client_Sell(Address_NFT, Id_NFT, Id_Bid).send({ from: account, value:0 })};
    
    prueba();"
                  
                  
     )
     
   })
   
   
   #5.15 CEO: Create a new offer
   
   onclick("B-OfferCEO", {
     
     runjs("
    
    const Address_NFT = document.getElementById('CEO-New-Offer1').value; 
    const Id_NFT = document.getElementById('CEO-New-Offer2').value; 
    const Price_NFT = document.getElementById('CEO-New-Offer3').value;
    
    console.log(Address_NFT);
    console.log(Id_NFT);
     
    const web3 = new Web3(Web3.givenProvider);
    
     var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.NFT_CEO_Offer_New(Address_NFT, Id_NFT, Price_NFT).send({ from: account, value:0 })};
    
    prueba();"
           
           
     )
     
   })
   
   
   #5.16 List enterprise offers
   
   wTable3 <- Waiter$new(id = "table3", color = transparent(.5), html = spin_loaders(15))
   
   observeEvent(input$"B-EnterpriseOffers",{
     
     wTable3$show() 
     
     Transactions<-content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="account", action="txlist", 
                           address=ContractAddress, apikey=ApiKeyEtherscan)))
     
     Transactions<-rbindlist(lapply(Transactions$result, as.data.frame.list))
     Transactions<-Transactions[txreceipt_status==1]
     
     NumberOfTransactions<-length(Transactions$hash)
     
     for( i in 1:NumberOfTransactions){
       
       Hash_Id<-content(GET(url="https://api-goerli.etherscan.io/api", query=list(module="proxy", 
                action="eth_getTransactionByHash",txhash=Transactions$hash[i],  apiKey=ApiKeyEtherscan)))
       
       if(i==1){
         Hash_Ids<-Hash_Id$result
       }
       
       else {
         Hash_Ids<-rbind(Hash_Ids, Hash_Id$result)
       }
       Sys.sleep(0.20)
     }
     
     Methods_Id<-data.table(Hash_Ids)
     Methods_Id<-Methods_Id[, 9]
     Methods_Id<-substr(Methods_Id$input, 1, 10)
     Methods_Id<-data.table(Methods_Id)
     
     E_NumberOffers<-Methods_Id[Methods_Id=="0x05496818"]
     E_NumberOffers<-as.numeric(E_NumberOffers[, Methods_Id:=.I][.N, 1])
     
     
     runjs(paste0("
     
    const web3 = new Web3(Web3.givenProvider);
    
    const a=[];
    
     var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
    
    for (let i = 0; i <", E_NumberOffers,"; i++) {
    
    a[i] = await contract1.methods.NFT_Enterprise_Offers(i).call(); 
    
    }
          
          Shiny.setInputValue('Enterprise_Offers',  a);
          Shiny.setInputValue('Enterprise_Offers_Id',  a.length-1);
    }
    
    console.log(a);
    
    prueba();
           
    "))
     
     
     output$table3<-DT::renderDT({
       
       a<-rbindlist(list(as.list(input$Enterprise_Offers)))
       
       a1<-7
       a2<-11
       aa<-data.table()
       
       for(i in 0: input$Enterprise_Offers_Id){
         
         aa<-rbind(a[, a1:a2],aa)
         a1=a1+12
         a2=a2+12
         
       }
       
       aa<-aa[, 1:4]
       setcolorder(aa[, Offer_Id:=as.integer(.N:1-1)],  c(5,1,2,3,4))
       
       #---------------------------------NFT IMAGE-----------------------------------------------------------------
       for(i in 1:nrow(aa)){
         
         NFT_ABI0<-data.table(content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="contract", action="getabi", address=aa[i,NFT], apikey=ApiKeyEtherscan)))$result)
         Sys.sleep(0.20)
         
         if(i==1){
           NFT_ABI<-data.table(content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="contract", action="getabi", address=aa[i,NFT], apikey=ApiKeyEtherscan)))$result)
           Sys.sleep(0.20)
         }
         
         else {
           NFT_ABI<-rbindlist(list(NFT_ABI, NFT_ABI0), fill = TRUE)
         }
         Sys.sleep(0.20)
       }
       
       aa<-cbind(aa, NFT_ABI)
       aa<-setnames(aa, "V1", "NFT_ABI")
       
       for(i in 1:nrow(aa)){
         runjs(paste0("
       const web3 = new Web3(Web3.givenProvider);
       var contract1 = new  web3.eth.Contract(", aa[i,NFT_ABI],", '",aa[i,NFT],"');
       async function prueba(){
       const myval = await contract1.methods.tokenURI(",aa[i,NFT_Id],").call();
       //console.log(myval);
       Shiny.setInputValue('Foto3",i,"',  myval);
       }
       prueba();"))
         
         NFT_URI0<-data.table(eval(parse(text=paste0("input$Foto3",i))))
         
         # JSON Cases:
         
         if( grepl("ipfs://", NFT_URI0[1], fixed = TRUE)==TRUE ){  
           NFT_JSON0<-paste0('https://', substr(NFT_URI0[1], 0, unlist(gregexpr('://', NFT_URI0[1]))[1]-1),
                             '.io/', substr(NFT_URI0[1], 0, unlist(gregexpr('://', NFT_URI0[1]))[1]-1), '/',
                             substr(NFT_URI0[1],  unlist(gregexpr('://', NFT_URI0[1]))[1]+3, nchar(NFT_URI0[1])))
           NFT_JSON0<-data.table(content(GET(url = NFT_JSON0[1]))$image) 
         } 
         
         else{ if( grepl("https://", NFT_URI0[1], fixed = TRUE)==TRUE ){
           NFT_JSON0<-NFT_URI0[1]
           NFT_JSON0<-data.table(content(GET(url = NFT_JSON0[1]))$image) 
         } 
           else{NFT_JSON0<-data.table("File not found")}
         }
         
         if( grepl("ipfs://", NFT_JSON0[1], fixed = TRUE)==TRUE ){
           NFT_JSON0<-NFT_JSON0[,V1:=paste0('https://', substr(V1, 0, unlist(gregexpr('://',V1))[1]-1),
                                            '.io/', substr(V1, 0, unlist(gregexpr('://', V1))[1]-1), '/',
                                            substr(V1, unlist(gregexpr('://', V1))[1]+3, nchar(V1)))]
         }
         
         else{ if( grepl("https://123", NFT_JSON0[1], fixed = TRUE)==TRUE ){
           NFT_JSON0<-NFT_JSON0 } 
           else{NFT_JSON0<-"https://etherscan.io/images/main/nft-placeholder.svg"}
         }
         
         NFT_JSON0<-NFT_JSON0[,V1:=paste0("<img src=", V1," height='52'></img>")]
         
         
         if(i==1) {
           NFT_URI<-NFT_URI0
           NFT_JSON<-NFT_JSON0
         }
         
         else {
           NFT_URI<-rbind(NFT_URI, NFT_URI0)
           NFT_JSON<-rbind(NFT_JSON, NFT_JSON0)
         }
       }
       
       NFT_URI<-setnames(NFT_URI, "V1", "NFT_URI", skip_absent=TRUE)
       NFT_JSON<-setnames(NFT_JSON, "V1", "NFT_JSON", skip_absent=TRUE)
       
       
       
       aa<-cbind(aa, NFT_JSON)
       aa[, NFT_ABI:=NULL]
       aa<-setnames(aa, "NFT_JSON", "NFT_Image")
       #---------------------------------NFT IMAGE-----------------------------------------------------------------
       
       
       DT::datatable(aa,  escape = FALSE, filter="top", style = "bootstrap4")
       
       
     }, filter="top")
     
     wTable3$hide()
     
   })   
   
   
   #5.17 Client: Buy listed NFT
   
   onclick("B-ClientBuy", {
     
     runjs("
    
    const Address_NFT = document.getElementById('Client-Buy1').value; 
    const Id_NFT = document.getElementById('Client-Buy2').value; 
    const Id_Offer = document.getElementById('Client-Buy3').value;
    const Price_Offer = document.getElementById('Client-Buy4').value;
    
    console.log(Address_NFT);
    console.log(Id_NFT);
    console.log(Id_Offer);
    console.log(Price_Offer);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.NFT_Client_Buy(Address_NFT, Id_NFT, Id_Offer).send({ from: account, value:Price_Offer })};
    
    prueba();"
           
     )
     
   })
   
   
   #5.18 List all enterprise shareholders
   
   wTable4 <- Waiter$new(id = "table4", color = transparent(.5), html = spin_loaders(15))
   
   observeEvent(input$"B-Shareholders1",{
     
     wTable4$show()
     
     Transactions<-content(GET(url = "https://api-goerli.etherscan.io/api", query = list(module="account", action="txlist", 
                           address=ContractAddress, apikey=ApiKeyEtherscan)))
     
     Transactions<-rbindlist(lapply(Transactions$result, as.data.frame.list))
     Transactions<-Transactions[txreceipt_status==1]
     
     NumberOfTransactions<-length(Transactions$hash)
     
     for( i in 1:NumberOfTransactions){
       
       Hash_Id<-content(GET(url="https://api-goerli.etherscan.io/api", query=list(module="proxy", 
                        action="eth_getTransactionByHash",txhash=Transactions$hash[i],  apiKey=ApiKeyEtherscan)))
       
       if(i==1){
         Hash_Ids<-Hash_Id$result
       }
       
       else {
         Hash_Ids<-rbind(Hash_Ids, Hash_Id$result)
       }
       Sys.sleep(0.20)
     }
     
     Methods_Id<-data.table(Hash_Ids)
     Methods_Id<-Methods_Id[, 9]
     Methods_Id<-substr(Methods_Id$input, 1, 10)
     Methods_Id<-data.table(Methods_Id)
     
     E_NumberShares<-Methods_Id[Methods_Id=="0x43a5a867"]
     E_NumberShares<-as.numeric(E_NumberShares[, Methods_Id:=.I][.N, 1])
     
     
     runjs(paste0("
     
    const web3 = new Web3(Web3.givenProvider);
    
    const a=[];
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
    
    for (let i = 0; i <", E_NumberShares,"; i++) {
    
    a[i] = await contract1.methods.ownerOf(i+1).call(); 
    
    }
          
          Shiny.setInputValue('Enterprise_Shares',  a);
          Shiny.setInputValue('Enterprise_Shares_Id',  a.length-1);
    }
    
    console.log(a);
    
    prueba();
           
    "))
     
     
     output$table4<-DT::renderDT({
       
       a<-rbindlist(list(as.list(input$Enterprise_Shares)))
       
       aa<-transpose(a)
       aa[, Share_Id:=as.integer(1:.N)]
       setnames(aa, 1, "Address")
       setcolorder(aa,  c(2,1))
       
       DT::datatable(aa,  escape = FALSE, filter="top", style = "bootstrap4")
       
     })
     
     wTable1$hide()
     
   })   

   
   #5.18 CFO: Pay Dividends
   
   onclick("B-DividendsCFO", {
     
     runjs("
    
    const Dividends = document.getElementById('CFO-Dividends').value;
    
    console.log(Dividends);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.Dividends_Pay(Dividends).send({ from: account, value:0 })};
    
    prueba();"
           
           
     )
     
   })
     
   
   #5.19 Founder: Change CEO
   
   onclick("B-FounderCEO", {
     
     runjs("
    
    const NewCEO = document.getElementById('Founder-CEO').value;
    
    console.log(NewCEO);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.Change_CEO(NewCEO).send({ from: account, value:0 })};
    
    prueba();"
           
           
     )
     
   }) 
   
   
   #5.20 Founder: Change CFO
   
   onclick("B-FounderCFO", {
     
     runjs("
    
    const NewCFO = document.getElementById('Founder-CFO').value;
    
    console.log(NewCFO);
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.Change_CFO(NewCFO).send({ from: account, value:0 })};
    
    prueba();"
           
           
     )
     
   }) 
   
   
   #5.21 Founder: End Enterprise
   
   onclick("B-FounderEnd", {
     
     runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
    async function prueba(){
          
    const accounts = await window.ethereum.request({ method: 'eth_requestAccounts' });
    const account = accounts[0];
          
    contract1.methods.End_Enterprise().send({ from: account, value:0 })};
    
    prueba();"
           
           
     )
     
   }) 
   
   
   #5.22 Financial transactions 
   
   wTable5 <- Waiter$new(id = "table5", color = transparent(.5), html = spin_loaders(15))
   
   observeEvent(input$"B-Financial1",{
     
     wTable5$show()
     
     runjs("
     
    const web3 = new Web3(Web3.givenProvider);
    const a=[];
    var aux=1;
    
    var contract1 = new  web3.eth.Contract([{\'inputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'constructor\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'approved\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Approval\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'indexed\':false,\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'ApprovalForAll\',\'type\':\'event\'},{\'anonymous\':false,\'inputs\':[{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Debit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount1\',\'type\':\'uint256\'},{\'indexed\':false,\'internalType\':\'string\',\'name\':\'_Credit\',\'type\':\'string\'},{\'indexed\':false,\'internalType\':\'uint256\',\'name\':\'_Amount2\',\'type\':\'uint256\'}],\'name\':\'New_Transaction\',\'type\':\'event\'},{\'anonymous\':false
    ,\'inputs\':[{\'indexed\':true,\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'indexed\':true,\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'Transfer\',\'type\':\'event\'},{\'inputs\':[],\'name\':\'Available_Stocks\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Buy_Share\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CEO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'CFO\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Cash_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCEO\',\'type\':\'address\'}],\'name\':\'Change_CEO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}
    ,{\'inputs\':[{\'internalType\':\'address\',\'name\':\'NewCFO\',\'type\':\'address\'}],\'name\':\'Change_CFO\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Amount\',\'type\':\'uint256\'}],\'name\':\'Dividends_Pay\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'End_Enterprise\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Enterprise\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Enterprise_Offers\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'Offer_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Offeror\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Bid_Id\',\'type\':\'uint256\'},{\'internalType\':\'address\',\'name\':\'Bidder\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'Founder\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'}
    ,{\'inputs\':[],\'name\':\'Info\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'Inventory\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'}],\'name\':\'NFT_Balance_Firm\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_CEO_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Accept\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comment\',\'type\':\'string\'}]
    ,\'name\':\'NFT_CEO_Offer_Review\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Buy\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Price\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Offer_New\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'Contract_NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'Bid\',\'type\':\'uint256\'}],\'name\':\'NFT_Client_Sell\',\'outputs\':[],\'stateMutability\':\'payable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Clients_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\'
    ,\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Bids\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\',\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'name\':\'NFT_Enterprise_Offers\',\'outputs\':[{\'internalType\':\'contract ERC721\',\'name\':\'NFT\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Id\'
    ,\'type\':\'uint256\'},{\'internalType\':\'uint256\',\'name\':\'NFT_Price\',\'type\':\'uint256\'},{\'internalType\':\'bool\',\'name\':\'Active\',\'type\':\'bool\'},{\'internalType\':\'string\',\'name\':\'Comments\',\'type\':\'string\'},{\'internalType\':\'address\',\'name\':\'Account\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'approve\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'}],\'name\':\'balanceOf\',\'outputs\':[{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'getApproved\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'owner\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'}],\'name\':\'isApprovedForAll\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\'
    ,\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'name\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'\',\'type\':\'bytes\'}],\'name\':\'onERC721Received\',\'outputs\':[{\'internalType\':\'bytes4\',\'name\':\'\',\'type\':\'bytes4\'}],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'ownerOf\',\'outputs\':[{\'internalType\':\'address\',\'name\':\'\',\'type\':\'address\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'}
    ,{\'internalType\':\'address\',\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'},{\'internalType\':\'bytes\',\'name\':\'_data\',\'type\':\'bytes\'}],\'name\':\'safeTransferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'operator\',\'type\':\'address\'},{\'internalType\':\'bool\',\'name\':\'approved\',\'type\':\'bool\'}],\'name\':\'setApprovalForAll\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'bytes4\',\'name\':\'interfaceId\',\'type\':\'bytes4\'}],\'name\':\'supportsInterface\',\'outputs\':[{\'internalType\':\'bool\',\'name\':\'\',\'type\':\'bool\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[],\'name\':\'symbol\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'tokenURI\',\'outputs\':[{\'internalType\':\'string\',\'name\':\'\',\'type\':\'string\'}],\'stateMutability\':\'view\',\'type\':\'function\'},{\'inputs\':[{\'internalType\':\'address\',\'name\':\'from\',\'type\':\'address\'},{\'internalType\':\'address\'
    ,\'name\':\'to\',\'type\':\'address\'},{\'internalType\':\'uint256\',\'name\':\'tokenId\',\'type\':\'uint256\'}],\'name\':\'transferFrom\',\'outputs\':[],\'stateMutability\':\'nonpayable\',\'type\':\'function\'}]
    ,'0x56305dd84673249753cFd7bE4B1e070000E0Cd25');
    
     async function prueba(){    
     
    const events = await contract1.getPastEvents('New_Transaction', {
    fromBlock: 0,
    toBlock: 'latest'
    });
    
    for (let i = 0; i <events.length; i++) {
    
    a[i] = events[i]; 
    
    }
          
    Shiny.setInputValue('Enterprise_Transactions',  a);
          
    console.log(a);
    
    aux=a.length;
    Shiny.setInputValue('Transactions_Q',  aux);
    console.log(aux);
    
    }
    
    prueba();
    
    ")
     
     
     output$table5<-DT::renderDT({
       
       a<-data.table(input$Enterprise_Transactions)
       
       for( i in 1:max(input$Transactions_Q, 1) ) {
         
         start<-9+20*(i-1)
         end<-12+20*(i-1)
         
         a1<-a[c(3+20*(i-1),start:end)]
         
         a2<-data.table("a"=a1[1], "b"=a1[2], "c"=a1[3], "d"=a1[4], "e"=a1[5])
         
         if (i==1) {
           
           a3<-a2
           
         } 
         
         else {
           
           a3<-rbindlist(list(a3, a2), use.names = TRUE)  
           
         }}
       
       setnames(a3, c("a.V1","b.V1","c.V1","d.V1","e.V1"), c("Block #","Account1", "Debit","Account2", "Credit"), skip_absent=TRUE)
       
       
       DT::datatable(a3,  escape = FALSE, filter="top", style = "bootstrap4")
       
     })
     
     wTable5$hide()
     
   }) 
    
}
