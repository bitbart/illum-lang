# Escrow

The Escrow contract regulates a payment between a buyer and a seller. 

The contract is split in the following phases:
- in the join phase, the buyer and the seller join the contract. 
  The buyer must provide a deposit in T;
- in the choice phase, the users try to find an agreement about the recipient of the deposit;
- in the redeem phase, if the users have found an agreement, then the chosen recipient 
  can redeem the deposit;
- after the redeem phase has passed, an escrow oracle is used to resolve the dispute. 
  Upon the payment of a fee, the oracle chooses the recipient, who can then redeem the remaining 
  part of the deposit.
