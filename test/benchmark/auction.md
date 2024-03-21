# Auction

The Auction contract allows a seller to create an auction. In the constructor, the seller must specify:
- its own address (`seller`),
- the `deadline` of the auction,
- the minimum bid (`min_bid`) of the auction,

After creation, the following actions are possible:
- bid: after the auction starts, any user can bid an amount of tokens T by transferring that amount to the contract. In the event of a raise, the contract returns the old maximum bid to the user who paid it;
- close: after the deadline, the seller can close the auction, and withdraw the highest bid. 
