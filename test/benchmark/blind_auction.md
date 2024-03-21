# Blind auction

The contract implements a [first-price blind auction](https://en.wikipedia.org/wiki/First-price_sealed-bid_auction) where users place their bids to buy an NFT. The winner is the user which places the highest bid. 
The auction is split in three phases:

- in the **bidding phase**, users place their sealed bids, and guarantee them by a deposit in tokens of type T. The deposit must cover the bid amount, and can be redeemed by non-winning users in the second phase;
- in the **reveal phase**, users reveal their bids. The contract state is updated to keep track of the current winner and highest bid. The non-winners who reveal their bids can redeem their deposits;
- in the **redeem phase**, the winner can withdraw the NFT and the difference between her deposit and her bid.