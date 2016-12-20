module UTest where

import           Lib
import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

tg = U.t "tg"
     (genesis 4)
     (Program [Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4)))]
              [Txn (Addr 1) Satoshi
              ,Txn (Addr 2) Satoshi
              ,Txn (Addr 3) Satoshi
              ,Txn (Addr 4) Satoshi])

sgbtx14 = selectGenesicBlockTxn 1 4

t1 = U.t "t1"
     sgbtx14
     (Txn (Choose [1]
                  (Program [Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4)))]
                           [Txn (Addr 1) Satoshi,Txn (Addr 2) Satoshi,Txn (Addr 3) Satoshi,Txn (Addr 4) Satoshi])
                  (Program [Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4)))]
                           [Txn (Addr 1) Disposal,Txn (Addr 2) Disposal,Txn (Addr 3) Disposal,Txn (Addr 4) Disposal]))
          (InL (Obligation (Addr 1) (Connection (Addr 2) (Connection (Addr 3) (Addr 4))))))

t2r = [Txn (Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4))))
           (Obligation (Addr 1) (Connection (Addr 2) (Connection (Addr 3) (Addr 4))))
      ,Txn (Addr 1) Satoshi,Txn (Addr 2) Satoshi,Txn (Addr 3) Satoshi,Txn (Addr 4) Satoshi]

t2 = U.t "t2"
     (step [sgbtx14])
     t2r

t3 = U.t "t3"
     (step t2r)
     []

test =
    runTestTT $ TestList $ tg ++ t1 ++ t2 ++ t3
