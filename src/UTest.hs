module UTest where

import           Lib
import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

sgbtx13 = selectGenesicBlockTxn 1 3

t1 = U.t "t1"
     sgbtx13
     (Txn (Choose [1] (Program [Addr 1,Addr 2,Addr 3]
                               [Txn (Addr 1) Satoshi
                               ,Txn (Addr 2) Satoshi
                               ,Txn (Addr 3) Satoshi])
                      (Program [Addr 1,Addr 2,Addr 3]
                               [Txn (Addr 1) Disposal
                               ,Txn (Addr 2) Disposal
                               ,Txn (Addr 3) Disposal]))
          (InL (Obligation (Addr 1)
                           (Connection (Addr 2) (Addr 3)))))

t2r = [Txn (Addr 1) (Obligation (Addr 1)
                                (Connection (Addr 2) (Addr 3)))
      ,Txn (Addr 1) Satoshi
      ,Txn (Addr 2) Satoshi
      ,Txn (Addr 3) Satoshi
      ]
t2 = U.t "t2"
     (step [sgbtx13])
     t2r
{-
t3 = U.t "t3"
     (step t2r)
     []
-}
test =
    runTestTT $ TestList $ t1 ++ t2 {- ++ t3 -}
