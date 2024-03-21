# Payment splitter

The Payment Splitter contract allows to split payments among a group of accounts. The sender does not need to be aware that the tokens will be split in this way, since it is handled transparently by the contract.

The split can be in equal parts or in any other arbitrary proportion. The way this is specified is by assigning each account to a number of shares. Of all the tokens that this contract receives, each account will then be able to claim an amount proportional to the percentage of total shares they were assigned. 

The distribution of shares is set at the time of contract deployment and can't be updated thereafter.

The contract follows a pull payment model. This means that payments are not automatically forwarded to the accounts but kept in this contract, and the actual transfer is triggered as a separate step by calling the `release()` function.

This is an HeLLUM adaptation of the [payment splitter](https://docs.openzeppelin.com/contracts/2.x/api/payment#PaymentSplitter) contract on OpenZeppelin.