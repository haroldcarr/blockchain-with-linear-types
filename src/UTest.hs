module UTest where

import           Lib
import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

tg = U.t "tg"
     (genesis 4)
     (Program [Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4)))]
              [Txn (Addr 1) Satoshi,Txn (Addr 2) Satoshi,Txn (Addr 3) Satoshi,Txn (Addr 4) Satoshi])

tb = U.t "tb"
     (burn 4)
     (Program [Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4)))]
              [Txn (Addr 1) Disposal,Txn (Addr 2) Disposal,Txn (Addr 3) Disposal,Txn (Addr 4) Disposal])

toba = U.t "toba"
       (obligation (Addr 1) (Connection (Addr 1) (Addr 2)))
       (Connection (Addr 1) (Connection (Addr 1) (Addr 2)))

tobi = U.t "tobi"
       (obligation (Isolation  (Addr 1) (Addr 2)) (Addr 2))
       (Connection (Connection (Addr 1) (Addr 2)) (Addr 2))

tobc = U.t "tobc"
       (obligation (Connection (Addr 1) (Addr 2)) (Addr 2))
       (Connection (Isolation  (Addr 1) (Addr 2)) (Addr 2))

sgbtx14 = selectGenesicBlockTxn 1 2 4

t1 = U.t "t1"
     sgbtx14
     (Txn (Choose [1]
                  (Program [Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4)))]
                           [Txn (Addr 1) Satoshi,Txn (Addr 2) Satoshi,Txn (Addr 3) Satoshi,Txn (Addr 4) Satoshi])
                  (Program [Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4)))]
                           [Txn (Addr 1) Disposal,Txn (Addr 2) Disposal,Txn (Addr 3) Disposal,Txn (Addr 4) Disposal]))
          (InL (obligation (Isolation  (Addr 1) (Addr 2))
                           (Connection (Addr 3) (Addr 4)))))


t2r = [Txn (Isolation (Addr 1) (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4))))
           (obligation (Isolation  (Addr 1) (Addr 2))
                       (Connection (Addr 3) (Addr 4)))
      ,Txn (Addr 1) Satoshi,Txn (Addr 2) Satoshi,Txn (Addr 3) Satoshi,Txn (Addr 4) Satoshi]

t2 = U.t "t2"
     (step sgbtx14)
     t2r

t3r = [Txn (Addr 1)
           (Connection (Addr 1) (Addr 2))
      ,Txn (Isolation (Addr 2) (Isolation (Addr 3) (Addr 4)))
           (Connection (Addr 3) (Addr 4))
      ,Txn (Addr 1) Satoshi,Txn (Addr 2) Satoshi,Txn (Addr 3) Satoshi,Txn (Addr 4) Satoshi]

t3 = U.t "t3"
     (stepl t2r)
     t3r

t4r = [Txn (Addr 1) (Connection (Addr 1) (Addr 2))
      ,Txn (Addr 2) (Addr 3)
      ,Txn (Isolation (Addr 3) (Addr 4)) (Addr 4)
      ,Txn (Addr 1) Satoshi,Txn (Addr 2) Satoshi,Txn (Addr 3) Satoshi,Txn (Addr 4) Satoshi]

t4 = U.t "t4"
     (stepl t3r)
     t4r

t5 = U.t "t5"
     (stepl t4r)
     t4r -- WRONG

test =
    runTestTT $ TestList $ tg ++ tb ++ toba ++ tobi ++ tobc ++
                           t1 ++ t2 ++ t3 ++ t4 ++ t5
