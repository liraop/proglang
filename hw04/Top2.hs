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

nonEmpty :: Reg -> Reg
         
nonEmpty Empty = Empty
nonEmpty (Or e1 e2) = Or nonEmpty(e1) nonEmpty(e2)
nonEmpty (Star e) = Then e Star e
