# Lottery

We specify a lottery where 2-players bet 1 token T each, and the winner redeems the whole pot. 

To achieve fairness, the lottery is structured in commit-reveal phases as follows:
- player1 joins the lottery by paying 1:T and committing to a secret
- player2 joins the lottery by paying 1:T committing to another secret
- player1 reveals the secret, or otherwise her bet can be redeemed by player2
- player2 reveals the secret, or otherwise her bet can be redeemed by player1
- the winner, who is determined as a function of the two revealed secrets, can redeem the whole pot.