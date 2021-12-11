
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.3;

import "https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/token/ERC721/ERC721.sol";
import "https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/token/ERC721/utils/ERC721Holder.sol";

contract BasicNFT is ERC721, ERC721Holder {

// 0) Variables Declaration:

string public Info = "Tokens represent stocks of an "
                     "NFT art dealer firm. Initially there are "
                     "100 unissued stocks with a unit price of "
                     "0.01 ethers.";
                             
address public Founder;
address public CFO;
address public CEO;
address public Enterprise;

uint public Available_Stocks=100;

struct NFT_Bid{
    ERC721 NFT;
    uint NFT_Id;
    uint NFT_Price;
    bool Active;
    string Comments;
    address Account;
}

NFT_Bid[] public NFT_Enterprise_Bids;
NFT_Bid[] public NFT_Enterprise_Offers;
NFT_Bid[] public NFT_Clients_Offers;

struct NFT_Transaction{
    uint Offer_Id;
    address Offeror;
    uint Bid_Id;
    address Bidder;
}

NFT_Transaction[] public Enterprise_Offers;

uint TokenCount=0;

// 1) Firm Construction:

constructor() ERC721("NFT Dealer", ":)") {

Founder = msg.sender;
CFO = msg.sender;
CEO = msg.sender;
Enterprise = address(this);
Available_Stocks=100;    

}

// 2) Contract Functions:

// 2.0) Buy Firm Stocks:

function Buy_Share() external payable {
require(msg.value == 0.01 ether, "Share value is 0.01 ethers");
require(Available_Stocks >= 1, "No more stocks available");
TokenCount=TokenCount+1;
_mint(msg.sender, TokenCount); 
 Available_Stocks=Available_Stocks-1;
} 

// 2.1) Pay Dividends:

function Dividends_Pay(uint Amount) external payable {
 require(Amount <= 1 ether, "Maximum amount is 1 ether");
 require(msg.sender==CFO, "This function is only for the CFO of the firm");
 for (uint i=0; i<100-Available_Stocks; i++){
 if(ownerOf(i+1)!=Enterprise){
 payable(ownerOf(i+1)).transfer(Amount);
 } 
 }
 }

// 2.2) Cash Balances:

function Cash_Firm() external view returns(uint) {
    return(address(this).balance);
}

function Cash_Personal() public view returns(uint) {
    return(msg.sender.balance);
}

// 2.3) NFT Balances:

function NFT_Balance_Firm(ERC721 Contract_NFT) external view returns(uint) {
    return(Contract_NFT.balanceOf(Enterprise));
}

function NFT_Balance_Personal(ERC721 Contract_NFT) public view returns(uint) {
     return(Contract_NFT.balanceOf(msg.sender));
}

// 2.4) Change Executives:

function Change_CFO(address NewCFO) public {
    require(msg.sender==Founder, "This function is only for the Founder of the firm");
    CFO=NewCFO;
}

function Change_CEO(address NewCEO) public {
    require(msg.sender==Founder, "This function is only for the Founder of the firm");
    CEO=NewCEO;
}

// 2.5) Buy/Sell NFT Art:

function NFT_Client_Offer_New(ERC721 Contract_NFT, uint Id, uint Price) public {
    require(Contract_NFT.ownerOf(Id)==msg.sender, "Not the owner of the NFT Token");
    NFT_Clients_Offers.push(NFT_Bid(Contract_NFT, Id, Price, true, "Under review", msg.sender));
}

function NFT_CEO_Offer_Review(uint Id, bool Accept, string memory Comment) public {
    require(msg.sender==CEO, "This function is only for the CEO of the firm");
    if(Accept==true){
    NFT_Clients_Offers[Id].Comments=Comment;
    NFT_Clients_Offers[Id].Active=true;
    NFT_Enterprise_Bids.push(NFT_Clients_Offers[Id]);
    Enterprise_Offers.push(NFT_Transaction(Id, NFT_Clients_Offers[Id].Account, NFT_Enterprise_Bids.length-1, Enterprise));
    } 
    else if(Accept==false)
    {NFT_Clients_Offers[Id].Comments=Comment;
    NFT_Clients_Offers[Id].Active=false;
    }
}

function NFT_Client_Sell(ERC721 Contract_NFT, uint Id, uint Bid) external payable {
     require(Contract_NFT==NFT_Enterprise_Bids[Bid].NFT, "Error on the NFT address");
     require(Id==NFT_Enterprise_Bids[Bid].NFT_Id, "Error on the NFT ID");
     require(NFT_Enterprise_Bids[Bid].NFT_Price<=Enterprise.balance, "The firm has not enought cash");
     require(NFT_Clients_Offers[Enterprise_Offers[Bid].Offer_Id].Active==true, "The offer is not active");
     Contract_NFT.safeTransferFrom(msg.sender, Enterprise, Id);
     payable(msg.sender).transfer(NFT_Enterprise_Bids[Bid].NFT_Price);
     NFT_Enterprise_Bids[Bid].Active=false;
     NFT_Clients_Offers[Enterprise_Offers[Bid].Offer_Id].Active=false;
}

function NFT_CEO_Offer_New(ERC721 Contract_NFT, uint Id, uint Price) public {
    require(msg.sender==CEO, "This function is only for the CEO of the firm");
    require(Contract_NFT.ownerOf(Id)==Enterprise, "Not the owner of the NFT Token");
    NFT_Enterprise_Offers.push(NFT_Bid(Contract_NFT, Id, Price, true, "-", Enterprise));
}

function NFT_Client_Buy(ERC721 Contract_NFT, uint Id, uint Bid) external payable {
     require(Contract_NFT==NFT_Enterprise_Offers[Bid].NFT, "Error on the NFT address");
     require(Id==NFT_Enterprise_Offers[Bid].NFT_Id, "Error on the NFT ID");
     require(msg.value==NFT_Enterprise_Offers[Bid].NFT_Price, "Error on the NFT price");
     require(NFT_Enterprise_Offers[Bid].Active==true, "The offer is not active");
     Contract_NFT.approve(CEO, Id);
     Contract_NFT.safeTransferFrom(Enterprise, msg.sender, Id);
     NFT_Enterprise_Offers[Bid].Active=false;
}

// 3) End Contract:

function End_Enterprise() external payable{
     require(msg.sender==Founder, "This function is only for the CEO of the firm");   
     selfdestruct(payable(Founder));
}

}