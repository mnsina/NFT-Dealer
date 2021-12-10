# NFT-Dealer
A public firm that buys and sells NFT tokens on the ethereum network through the use of an ERC721 smart contract.

## Enterprise Characteristics:

-Stocks: There are 100 unissued stock without voting rights, each has a unit price of 0.01 ethers. \
\
-Founder: Can change the CFO, CEO and end the enterprise. \
\
-CEO: Accepts or rejects clients offers and decides the price at which the NFT's bought by the firm will be sold. \
\
-CFO: Decides the amount and when dividends are pay to shareholders. \
\
-Enterprise Bids: All enterprise bids are linked to a previous client offer.\
\
-Enterprise Offers: All the NFT's that the enterprise offers were bought previously to a client.\
\
-Clients Bid: All client bids are linked to a previous enterprise offer. \
\
-Clients Offers: All client offers are reviewed by the CEO before being accepted or rejected. 

## Instructions to Sell NFT:

-1): Use the function "NFT_Client_Offer_New" to offer a NFT at a specific price.\
\
-2): Check that the CEO has reviewed the offer using the function "NFT_Clients_Offers".  \
\
-3): After the offer is accepted approve control of the NFT token to the enterprise address (NFT Original Contract) and then use the function "NFT_Client_Sell" with the bid id that the CEO wrote on the previous step. 

## Instructions to Buy NFT:

-1): Use the function "NFT_Enterprise_Offers" to search active offers.\
\
-2): Use the function "NFT_Client_Buy" using the information of the previous step.

## Enterprise Etherscan Link:
https://rinkeby.etherscan.io/address/0xf7659ece6c761cd39d1c739ef72668ce381f8305#writeContract

## Available NFT for sale:
-1) BAYC Token #1:\
https://rinkeby.etherscan.io/address/0x6c4a156c9000dfd03f846c68b175bc41344e9b19#readContract

## Contact Info:
#### mnsina@uc.cl
