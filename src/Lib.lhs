> module Lib where

REPRESENTATION

`Program` represents a blockchain state.

`[Expr]` represents resources available on the blockchain.

`[Transaction]` represents transactions in progress.

e.g.,

`Program [1blkchnaddr] [Txn 1blkchnaddr (M . Satoshi)]`

represents the genesis block: where `1blkchnaddr` has been assigned M Satoshi, more accurately written

`Program [addr1 * ... * addrM] [Txn addr1, Satoshi, ..., Txn addrM, Satoshi]`

> genesis = gb Satoshi

and

`Program [1blkchnaddr] [Txn 1blkchnaddr Disposal]`

represents disposing the assets in `1blkchnaddr`, more accurately written

`Program [addr1 * ... * addrM] [Txn addr1 Disposal, ..., Txn addrM Disposal]`

> burn = gb Disposal

> data Program
>   = Program     [Expr]    [Transaction]   -- (e1, ..., em) {t1; ...; tn}
>  deriving (Eq, Read, Show)

> type Address = Int

> data Expr
>   = Satoshi                               -- currency units
>   | Addr        Address                   -- x
>   | Isolation   Expr      Expr            -- e * e
>   | Connection  Expr      Expr            -- e # e
> --  | Obligation  Expr      Expr            -- e lollipop e
>   | Choose      [Address] Program Program -- menu
>   | InL         Expr                      -- selection
>   | InR         Expr                      -- selection
>   | Storage     Expr                      -- ? e
>   | Disposal                              -- _
>   | Contraction Expr      Expr            -- e @ e
>   | Replication [Address] Program         -- !(x1, ..., xn){p}
>   deriving (Eq, Read, Show)

> data Transaction
>   = Txn         Expr      Expr            -- txn(e1, e2)
>   deriving (Eq, Read, Show)

OPERATIONAL SEMANTICS

> stepl [] = []
> stepl (x:xs) = step x ++ stepl xs

> -- transaction
> -- step (Txn e1 (Addr x) : Txn (Addr x') e2 : ts) | x == x'
> --  = Txn e1 e2 : ts

> -- pair
> step (Txn (Isolation  e1 e1')
>           (Connection e2 e2'))
>   = [ Txn e1 e2, Txn e1' e2' ]

> -- left
> step (Txn (Choose (x:xs) (Program (e:es) pts)
>                          _)
>           (InL e'))
>   = Txn e e' : pts ++ mkTxns xs es

> -- right
> step (Txn (Choose (x:xs) _
>                          (Program (e:es) pts))
>           (InR e'))
>   = Txn e e' : pts ++ mkTxns xs es

> -- read
> step (Txn (Replication xs (Program (e:es) pts))
>           (Storage e'))
>   = Txn e e' : mkTxns xs es

> -- dispose
> step (Txn (Replication xs _) Disposal)
>   = mkTxns' xs [ Disposal | x <- [ 1 .. ]]

> -- copy
> step (Txn (Replication xs p)
>           (Contraction e1 e2))
>   = undefined

> step x = [x]

INTERPRETATION

Executing is joining the two expressions in a transaction.
E.g., to send I < M Satoshi to bddr1 *...*bddrI, in the
context of the genesis block, the genesis block must be
an expression.

> genesisExpr m = Choose [1] (genesis m) (burn m)

Then form a spend expression bddr1 ∗ . . . ∗ bddrI ⊸ addrI+1#addrM
to consume I satoshi from genesis block addresses addr1 through addrI
and deposit them in addr1 through addrI.

To do so, create transaction that selects genesis block from menu of blockchain states

> selectGenesicBlockTxn begin middle end =
>   Txn (genesisExpr end)
>       (InL (obligation (mkI begin middle) (mkC (begin + middle) end)))

Utilities

> gb gOrB m = case gOrB of
>   Satoshi  -> go
>   Disposal -> go
>   _        -> error "no"
>  where
>   go    = Program [addrs] txns
>   ms    = [ 1 .. m ]
>   addrs = mkI  1 m
>   txns  = fmap (\a -> Txn (Addr a) gOrB) ms

> mkTxns  xs es | length xs == length es = mkTxns' xs es
>               | otherwise              = error "lengths not the same"
>
> mkTxns' xs es = fmap (\(x,e) -> Txn (Addr x) e) (zip xs es)

> mkI = mkIorC Isolation
> mkC = mkIorC Connection
> mkIorC iOrC start end =
>   foldr (\x y -> iOrC (Addr x) y)
>         (Addr end)
>         [ start .. end - 1 ]

> obligation e1 e2 =
>   Connection (xnot e1) e2
>  where
>   xnot a@(Addr _) = a
>   xnot (Isolation  ie1 ie2) = Connection (xnot ie1) (xnot ie2)
>   xnot (Connection ce1 ce2) = Isolation  (xnot ce1) (xnot ce2)
