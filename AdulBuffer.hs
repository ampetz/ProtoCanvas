module AdulBuffer where

import Data.List

import qualified Adul as A
import qualified ISpi as I
import ProtoState
import ParseFile

xx :: I.Gamma -> A.Message -> A.Message
xx gamma (A.Message a b c piIn) =
  let piOut = I.subIfVar' piIn gamma in (A.Message a b c piOut)

addMessageAt' :: (Pos,MessageD) -> Proto ()
addMessageAt' (pos,m) = addMessageAt pos m
  
convertMessage :: A.Message -> (Pos,MessageD)
convertMessage (A.Message pos from to pi) =
  (pos, MessageD from to (show pi))

mList = A.toMessages A.inst_m2_shared []

sortPairs :: [(Pos,MessageD)] -> [(Pos,MessageD)]
sortPairs ps = sortBy pred ps
 where pred :: (Pos,MessageD) -> (Pos,MessageD) -> Ordering
       pred (i,_) (j,_) = compare i j
