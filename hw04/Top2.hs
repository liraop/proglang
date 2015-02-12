-------------------------------------------------------------------------- 
--									--
--	Top2.hs								--
--									--
--	Top level file in the NFA library.				--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--	as munged by Jim Royer, 2012					--
--									--
-------------------------------------------------------------------------- 

-- Modified by Pedro de Oliveira Lira - pdeolive@syr.edu

module Top2 where

import BuildNfa2
import ImplementNfa
import NfaLib
import NfaToDfa
import NfaMisc
import NfaTypes
import RegExp2
import Matches2
import DrawNfa

nonempty :: Reg -> Reg      
nonempty Empty = Empty
nonempty Epsilon = Empty
nonempty (Literal x) = Literal x
nonempty (Or e1 e2) =  Or (nonempty e1) (nonempty e2)
nonempty (Star e) = Then (nonempty(e)) (Star e)
nonempty (Then e1 e2) = Or (Then e1 (nonempty e2)) (Then (nonempty e1) e2)
nonempty (Plus e) =  Then (nonempty(e)) (Star e)
nonempty (Opt e) = nonempty(e)
