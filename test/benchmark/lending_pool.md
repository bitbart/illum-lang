# Lending pool

A lending pool allows users to deposit their tokens into the contract, and later use them as a collateral to borrow other tokens. Such debts accrue interests over time. If one’s debt grows beyond a certain threshold, then a portion of their collateral is put on sale by the lending pool. Namely, other users can now buy the collateral at a discounted price, effectively repaying a part of the debt (*liquidation*), until it is again under the threshold.

We represent users' collateral as a map from user accounts to nonnegative numbers, modelling the reward given by the contract to users for adding liquidity. For simplicity, we assume a single token type T for all transactions 
(the reward, called *minted token* in the DeFi jargon, is not an actual token). 

Lending pools feature several operations: depositing and redeeming collateral, borrowing and repaying debts, accruing interest, liquidating others' debts. 

Users can deposit tokens in the LP, obtaining in return virtual tokens minted by the contract. More precisely, upon a deposit of x:T in a pool with reserves of n:T, the user will receive x*X(n) minted tokens, where the exchange rate is given by the pure function `X()`.

Our Lending Pool manages a single token type T. The use of minted tokens is twofold. On the one hand, they are an incentive to lend: users deposit speculating that the minted tokens will be redeemable for a value greater than the original deposit. On the other hand, they are used as a collateral when borrowing tokens: namely, users can obtain a loan only if they have enough collateralization, that is given by the pure function `C()`.

The borrow action requires that the user collateralization after the action is above a given threshold `Cmin`.

Users can redeem their minted tokens for units of T, where the actual amount is obtained by applying the exchange rate. Also the redeem requires that the collateralization is above the threshold.

An oracle can accrue interests on loans from time to time. This is done by multiplying the interest rate ir by the factor Imul> 1. Note that interest accrual may make some borrowers undercollateralized, exposing them to liquidations. 

A liquidate action allows anyone to repay part of the debt of an undercollateralized user, and obtain as a reward part of their minted tokens. The multiplication factor `Rliq` incentivizes liquidations.