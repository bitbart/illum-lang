# Voting

The contract implements an election, which is organized in two phases:
- an **application phase**, where users can apply as a candidate;
- a **voting phase**, where users who have received (off contract) a voting token VT, can spend units of their tokens to vote the candidates.

The user who has obtained the majority of votes wins a winner token WT.  

This is an HeLLUM adaptation of the [voting contract](https://developer.algorand.org/solutions/example-permissioned-voting-stateful-smart-contract-application/) on Algorand.