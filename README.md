# ILLUM: an Intermediate-Level Language for the UTXO Model

The project implements a compiler from the HeLLUM contract language to the ILLUM intermediate language. 
HeLLUM is a subset of Solidity that is suitable to be executed on UTXO blockchains like Cardano.

For example, the following is an [Auction contract](test/auction.hll) in HeLLUM:
```
contract Auction {
    uint deadline;
    uint min_bid;
    address winner;  
    address seller;

    constructor(address a, uint d, uint m) {
        seller = a;
        deadline = d;
        min_bid = m;
    }
    
    function bid(uint v, address a) input(v:T) {
        require v >= min_bid;
        require v > balance(T);     // the current bid is greater than the previous ones 
        require a != address(0);
        
        // the previous maximum bid is returned to the previous winner
        winner.transfer((balance(T)-v):T);
        
        // the new winner is set to the current (highest) bidder
        winner = a;
    }
        
    function close()
        auth(seller) 
        after(deadline) 
    {
        seller.transfer(balance(T):T);
    }
}
```

## HeLLUM contracts

The repository includes a benchmark of common Solidity smart contracts, implemented in HeLLUM:
- [Crowdfund](test/benchmark/crowdfund.hll)
- [Auction](test/benchmark/auction.hll)
- [Payment splitter](test/benchmark/payment_splitter.hll)
- [Vault](test/benchmark/vault.hll)
- [Automated Market Maker](test/benchmark/amm.hll)
- [Voting](test/benchmark/voting.hll)
- [Vesting wallet](test/benchmark/vesting_wallet.hll)
- [Escrow](test/benchmark/escrow.hll)
- [King of the Hill](test/benchmark/king_of_the_hill.hll)
- [Blind auction](test/benchmark/blind_auction.hll)
- [Lending pool](test/benchmark/lending_pool.hll)
- [Lottery](test/benchmark/lottery.hll)

## Installation and setup

We give below minimal instructions to setup a local installation of OCaml on Linux.
See [here](https://ocaml.org/docs/up-and-running) for instructions on other OSs.

First, install opam, the OCaml official package manager:
```bash
sudo apt install opam
```
Then, you must initialize opam. This installs OCaml and creates a default switch:
```bash
opam init --bare -a -y
```
Here we assume you will work on the default switch. To check that a switch actually exists:
```bash
opam switch list
```
In case the previous command shows an empty list, you must manually create a switch:
```bash
opam switch create illum ocaml-base-compiler.4.14.0
```

The following command updates environment variables, to make OCaml commands available on the current switch:
```bash
eval $(opam env)
```

Finally, we need a few extra OCaml packages:
```bash
opam install -y dune ocaml-lsp-server odoc ocamlformat menhir ppx_inline_test digestif
```
In particular, this installation includes:
- [**dune**](https://dune.readthedocs.io/), a build system for OCaml projects, similar to make;
- [**Menhir**](http://gallium.inria.fr/~fpottier/menhir/), a parser generator;
- [**ppx_inline_test**](https://github.com/janestreet/ppx_inline_test), a tool for writing in-line tests in OCaml;
- [**digestif**](https://github.com/mirage/digestif), for cryptographic hashes. 

## Running the ILLUM compiler

After cloning the repository, you can run a collection of tests through the following command, launched from the `illum-lang` directory:
```bash
dune test 
```
If everything is ok, the command will produce no output.

To run the HeLLUM compiler:
```
dune exec illum hllc test/auction.hll
```
If successful, the command produces a set of ILLUM clauses.
For example, the clauses resulting from the compilation of the Auction contract are:
```
clause constructor_run(deadline,min_bid,winner,seller,bal_T; a,d,m) {
  precond_wallet: bal_T:T
  precond_if: true
  process: 
    call constructor_next(d,m,winner,a,bal_T)
}
clause constructor_next(deadline,min_bid,winner,seller,bal_T; ) {
  precond_wallet: bal_T:T
  precond_if: true
  process: 
    call bid(deadline,min_bid,winner,seller,bal_T)
    auth(seller) afterAbs(deadline) : call close(deadline,min_bid,winner,seller,bal_T)
}
clause bid_run(deadline,min_bid,winner,seller,bal_T; v,a) {
  precond_wallet: bal_T+v:T
  precond_if: (v>=min_bid && (v>bal_T+v)) && a!=address(0)
  process: 
    call( Pay(winner,(bal_T+v)-v,T) | bid_next(deadline,min_bid,a,seller,(bal_T+v)-bal_T) )
}
clause bid_next(deadline,min_bid,winner,seller,bal_T; ) {
  precond_wallet: bal_T:T
  precond_if: true
  process: 
    call bid(deadline,min_bid,winner,seller,bal_T)
    auth(seller) afterAbs(deadline) : call close(deadline,min_bid,winner,seller,bal_T)
}
clause close_run(deadline,min_bid,winner,seller,bal_T; ) {
  precond_wallet: bal_T:T
  precond_if: true
  process: 
    call( Pay(seller,bal_T,T) | close_next(deadline,min_bid,winner,seller,0) )
}
clause close_next(deadline,min_bid,winner,seller,bal_T; ) {
  precond_wallet: bal_T:T
  precond_if: true
  process: 
    call bid(deadline,min_bid,winner,seller,bal_T)
    auth(seller) afterAbs(deadline) : call close(deadline,min_bid,winner,seller,bal_T)
}
clause Pay(a,v,t; ) {
  precond_wallet: v:t
  precond_if: true
  process: 
    send(v:t -> a)
}
clause Check(b; ) {
  precond_wallet: 
  precond_if: b
  process: 
    send()
}
```