# NFT-Dealer
A public firm that buys and sells NFT tokens on the Polygon network through the use of an ERC721 smart contract.

## Enterprise Characteristics:

1) Stocks: There are 100 unissued stocks without voting rights, each has a unit price of 0.01 Matic. 
 
2) Founder: Can change the CFO, CEO and end the enterprise. 
 
3) CEO: Accepts or rejects clients offers and decides the price at which the NFT's bought by the firm will be sold. 
 
4) CFO: Decides the amount and when dividends are pay to shareholders. 
 
5) Enterprise Bids: All enterprise bids are linked to a previous client offer.
 
6) Enterprise Offers: All the NFT's that the enterprise offers were bought previously to a client.
 
7) Clients Bid: All client bids are linked to a previous enterprise offer. 
 
8) Clients Offers: All client offers are reviewed by the CEO before being accepted or rejected. 

## Instructions to Sell NFT:

1) Use the function "NFT_Client_Offer_New" to offer a NFT at a specific price.
 
2) Check that the CEO has reviewed the offer using the function "NFT_Clients_Offers".  
 
3) After the offer is accepted approve control of the NFT token to the enterprise address (NFT Original Contract) and then use the function "NFT_Client_Sell" with the bid id that the CEO wrote on the previous step. 

## Instructions to Buy NFT:

1) Use the function "NFT_Enterprise_Offers" to search active offers.
 
2) Use the function "NFT_Client_Buy" using the information of the previous step.

## Enterprise Polygonscan Link:
https://polygonscan.com/address/0x9d42afbf7461ae6f40db39e28b5baebaefc710ed

## DApp Link:
https://mnsina.shinyapps.io/Ethereum/

## Contact Info:
#### mnsina@uc.cl
