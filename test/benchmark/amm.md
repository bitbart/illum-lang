# Constant-product AMM

The AMM (Automated Market Maker) contract allows users to deposit, redeem, and swap tokens.
The constructor takes an initial amount of tokens of types T0 and T1, and an amount of
minted tokens M01.

The function `addLiquidity` allows users to deposit tokens in the AMM, provided that the exchange rate 
between T0 and T1 is preserved. Based the deposited amount, the AMM transfers an amount of minted
tokens to the liquyidity provider.

The function `removeLiquidity` allows liquidity providers to redeem their minted tokens, always
preserving the exchange rate.

The function `swap0` allow users to sell tokens T0 in exchange for tokens T1. The exchange rate
is determined so to preserve the product between the token reserves in the AMM (as in Uniswap v2).
The function also allows users to specify the minimum desired output amount.

The function `swap1` does the same, allowing to sell tokens T1 in exchange for tokens T0.

The AMM implementation is based on Uniswap v2 
(https://github.com/Uniswap/v2-periphery/blob/master/contracts/UniswapV2Router02.sol#L77).