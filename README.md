# ILLUM: an Intermediate-Level Language for the UTXO Model

The project implements a compiler from the HeLLUM contract langauge to the ILLUM intermediate language. 
HeLLUM is a subset of Solidity that is suitable to be executed on UTXO blockchains.

For example, the following is an [Auction contract](test/auction.hll) in HeLLUM:
```
contract Auction {
    uint t; 	// timeout
    uint m; 	// min bid
    address W; 	// winner  
    address A; 	// owner

    constructor(address owner, uint timeout, uint min_bid) {
        t = timeout; 
        m = min_bid;
        A = owner;
        W = address(0);
    }
    
    function bid(uint v, address X) input(v:T) {
        require(X!=address(0) && v>=m && v>balance(T));
        if (W!=address(0))
            W.transfer((balance(T)-v):T);
        W = X;
    }
        
    function close() 
        auth(A) 
        after(t) 
    {
        A.transfer(balance(T):T);
    }
}
```

## HeLLUM contracts

The repository includes a benchmark of common Solidity smart contracts, implemented in HeLLUM:
- [Auction](test/auction.hll)
- [Constant-product AMM](test/amm.hll)
- [Crowdfund](test/crowdfund.hll)
- [Payment splitter](test/payment_splitter.hll)

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
This creates a switch for the LIP course with the given version of the OCaml compiler.

The following command updates environment variables, to make OCaml commands available on the current switch:
```bash
eval $(opam env)
```

Finally, install a few extra OCaml packages:
```bash
opam install -y dune ocaml-lsp-server odoc ocamlformat menhir ppx_inline_test
```
In particular, this installation includes:
- [**dune**](https://dune.readthedocs.io/), a build system for OCaml projects, similar to make;
- [**Menhir**](http://gallium.inria.fr/~fpottier/menhir/), a parser generator;
- [**ppx_inline_test**](https://github.com/janestreet/ppx_inline_test), a tool for writing in-line tests in OCaml.

## Running the ILLUM compiler

After cloning the repository, you can run a collection of tests through the command from the `illum-lang` directory:
```bash
dune test 
```
If everything is ok, then the command will produce no output.

To run the HeLLUM compiler:
```
dune exec illum hllc test/auction.hll
```
If successful, the command produces a set of ILLUM clauses.
For example, the clauses resulting from the compilation of the Auction contract are:
```
clause bid(t,m,W,A,bal_T; v,X) {    
  wallet: bal_T+v:T
  require: ((X!=address(0)) && (v>=m)) && (v>balance(T))
  branch: 
    call( Check(W!=address(0)) || Pay(W,balance_pre(T)-v,T) || Post-bid(t,m,X,A,(bal_T+v)-((bal_T+v)-v)) )
    call( Check(!(W!=address(0))) || Post-bid(t,m,X,A,bal_T+v) )
}
clause Post-bid(t,m,W,A,bal_T; ) {
  wallet: 
  require: true
  branch: 
    call bid(t,m,W,A,bal_T)
    auth(A) afterAbs(t) : call close(t,m,W,A,bal_T)
}
clause close(t,m,W,A,bal_T; ) {
  wallet: bal_T:T
  require: true
  branch: 
    call( Pay(A,balance_pre(T),T) || Post-close(t,m,W,A,bal_T-bal_T) )
}
clause Post-close(t,m,W,A,bal_T; ) {
  wallet: 
  require: true
  branch: 
    call bid(t,m,W,A,bal_T)
    auth(A) afterAbs(t) : call close(t,m,W,A,bal_T)
}
clause Pay(a,v,t; ) {
  wallet: v:t
  require: true
  branch: 
    send(v:t -> a)
}
clause Check(b; ) {
  wallet: 
  require: b
  branch: 
    send(0:T -> Null)
}
```