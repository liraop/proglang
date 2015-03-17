-- LCcompiler.hs
--   Version 4 (Thu Feb 26 11:32:41 EST 2015)
-- STUDENT :  Pedro de Oliveira Lira - pdeolive@syr.edu

import Data.Word   (Word8)
import Data.Char   (ord)
import LC2
import LCParser
import LCvm

fixMe fun = error ("Please fix me: " ++ fun)

---------------------------------------------------------------------------
-- the compiler
---------------------------------------------------------------------------

compile :: Command -> [AsmCode]
-- compile p = a compilation of the program p
compile p = (transC p)++[Halt]

transA :: AExp -> [AsmCode]
-- transA ae = a compilation of the arithemetic expression ae
transA (AConst n)           = [Push n]
transA (Val loc)            = [Fetch loc]
transA (ABin Plus ie1 ie2)  = transA ie1 ++ transA ie2 ++ [Iadd]
transA (ABin Minus ie1 ie2) = transA ie1 ++ transA ie2 ++ [Isub]
transA (ABin Times ie1 ie2) = transA ie1 ++ transA ie2 ++ [Imult]

transB :: BExp -> [AsmCode]
-- transB be = a compilation of the boolean expression be
transB (BConst b)            = [Push (boolToWrd b)]

-- HW 7 ----------------------- PROBLEM 5 A -------------------------------------
transB (Not be)              = transB be ++ [Push 1, Ilt]

transB (Compare Leq ie1 ie2) = transA ie2 ++ transA ie1 ++ [Ilt, Push 1, Ilt]

transB (Compare Geq ie1 ie2) = transA ie1 ++ transA ie2 ++ [Ilt, Push 1, Ilt]
---------------------------------------------------------------------------------

transB (Compare Eq ie1 ie2)  = transA ie1 ++ transA ie2 ++ [Isub,Push 1,Ilt]
transB (Compare Neq ie1 ie2) = [Push 0] ++ transA ie1 ++ 
                                  transA ie2 ++ [Isub,Ilt]
transB (Compare Lt ie1 ie2)  = transA ie1 ++ transA ie2 ++ [Ilt]
transB (Compare Gt ie1 ie2)  = transA ie2 ++ transA ie1 ++ [Ilt]

transC :: Command -> [AsmCode]
-- transC stm = a compilation of the statement stm
transC Skip              = []
transC (Set loc ie)      = transA ie ++ [Store loc,Pop]
transC (Seq stm1 stm2)   = transC stm1 ++ transC stm2 
transC (If be stm1 stm2) 
    = let thenpart = transC stm1  
          elsepart = transC stm2
      in transB be ++ [Jz (3+codeLen thenpart)] ++ thenpart 
             ++ [Jmp (1+codeLen elsepart)] ++ elsepart
transC (While be cmd)    
    = let tst  = transB be
          body = transC cmd
      in tst ++ [Jz (3+codeLen body)] ++ body
             ++ [Jmp (-(3+codeLen body+codeLen tst))]

-- For Homework 7 -- PROBLEM 6 A ------------------------------------------
transC (For loc cmd) 
   = transC (While (Compare Neq (Val loc) (AConst 0))(Seq cmd (Set loc (ABin Minus (Val loc) (AConst 1)))))

---------------------------------------------------------------------------
-- Utilities: printing assemby version of programs and VM runs
---------------------------------------------------------------------------

showCompiled :: String -> IO ()
-- Prints the assembly version of a compiled program
showCompiled prog = prnAsm (compile (cparse prog))

clg :: String -> IO ()
-- clg program = compile, load, and go (i.e., run the program)
clg prog = let p     = cparse prog      -- parse the program
               ap    = compile p        -- compile the program
           in stepRun ap

clg' :: String -> IO ()
-- Prints the program, the assembly version, and the run of the 
--   compiled program.
clg' prog = do { putStrLn ("The program:\n\t"++prog++"\n"++"compiles to")
               ; showCompiled prog
               ; putStrLn "Here is the run:"
               ; clg prog
               }

-- sample integer expressions
ie0 = "4"
ie1 = "3+8"
ie2 = "(1+2)*(3+2*(10-2))"

-- Sample commands
-- Try 
--   clg c1
--   clg' c1
-- and keep hitting return to get the next step displayed.
c1 = "{ x1:=12; skip; x2:=9 }"
c2 = "{ x1:=12; skip; x2:=9; x2:=val(x2)-1 }"
c3 = "if (val(x2)==0) then x0:=10 else x0:=20"
c4 = "if not (val(x2)==0) then x0:=10 else x0:=20"
c5 = "{ x0:=4; x1:=1; while (val(x0)>0) do { x1:=val(x1)*val(x0); "
     ++ "x0:=(val(x0)-1) } }"

-- Try, for example: (clg' (c6c 0)) and (clg' (c6c 12))
c6a, c6b, c6c, c6d, c6e, c6f :: Int -> String
c6a n = "x2 := " ++show n++"; if (val(x2)>0) then x0:=10 else x0:=20"
c6b n = "x2 := " ++show n++"; if (val(x2)>=0) then x0:=10 else x0:=20"
c6c n = "x2 := " ++show n++"; if (val(x2)<=0) then x0:=10 else x0:=20"
c6d n = "x2 := " ++show n++"; if not(val(x2)>0) then x0:=10 else x0:=20"
c6e n = "x2 := " ++show n++"; if not(val(x2)>=0) then x0:=10 else x0:=20"
c6f n = "x2 := " ++show n++"; if not(val(x2)<=0) then x0:=10 else x0:=20"

c7 = "{ x0 := 3; while (val(x0)>0) do x0 := val(x0)-1 }"
c8 = "{ x0:=4; x1:=1; while (val(x0)>0) do "
     ++ "{ x1:=val(x1)*val(x0); x0:=(val(x0)-1) } }"

c9 n = "{ x0 := "++show n++"; x1 := val(x0); for x0 do x1 := val(x1)-1 }"
c10 = "{ x0:=4; x1:=1; for x0 do x1:=val(x1)*val(x0) }"

-- Note: c8 and c10 are different ways of computing 4!.
-- In each one, the register x1 should end up with value 24.

notTest1, notTest2 :: IO ()
leqTest, geqTest :: Wrd -> Wrd -> IO ()

-- When running notTest1 the final configuration should have 
--   and empty stack and x1=1.
-- When running notTest2 the final configuration should have 
--   and empty stack and x1=2.
notTest1 = clg' ("if ( not tt) then x1 := 2 else x1 := 1")
notTest2 = clg' ("if ( not ff) then x1 := 2 else x1 := 1")

-- (leqTest m n) should end up with an empty stack and
--   if m<=n then x1=1 else x1=2.
leqTest m n = clg (concat ["if ( ",show m," <= ",show n,
                           ") then x1 := 1 else x1 := 2"])

-- (geqTest m n) should end up with an empty stack and
--   if m>=n then x1=1 else x1=2.
geqTest m n = clg (concat ["if ( ",show m," >= ",show n,
                           ") then x1 := 1 else x1 := 2"])


--------------------------- MY TESTS --------------------------------------

---- TESTS FOR PROBLEM 5 b) 

-- The reg x1 will end with value 1 because it evaluates 2 > 1 as true but not makes it false
myNotTest1 = clg ("if ( not (2 > 1) ) then x1 := 0 else x1 := 1")

-- The reg x1 will end with value 1 because when push 256, an overflow happens, and-- it's pushed 0, then it compared with 254, evaluating false but the not makes it true.
myNotTest2 = clg (" if ( not (256 > 254)) then x1 := 1 else x1 := 0")

-- No regs != 0 because 256 when pushed is overflow, then is stored 0, which when compared to 0 is true.
myLeqTest1 = clg ("if ( 256 <= 0) then x1 := 0 else x1 := 1")

-- The reg x1 will end with value 1 because 256 when pushed is overflow, then is stored 0, which when compared to 255 is false.
myLeqTest2 = clg ("if ( 255 <= 256) then x1 := 0 else x1 := 1")

-- The reg x1 will end with value 1 because 256 when pushed is overflow, then is stored 0, which when compared to 255 is false.
myGeqTest1 = clg ("if ( 256 >= 255) then x1 := 0 else x1 := 1")

-- No regs != 0 because 256 when pushed is overflow, then is stored 0, which when compared to 0 is true.
myGeqTest2 = clg ("if ( 256 >= 0) then x1 := 0 else x1 := 1")
-----------------------------

------ TESTS FOR PROBLEM 6 iii)

--  x1's final value will be 13. For will add 1 to x1 13 times. 
myForTest1 = clg ("{ x0 := 13; x1 := 0; for x0 do x1 := val(x1)+1 }")


-- x1's final value will be 1. 254 is pushed to x1 then it will incremented by 1
-- 3 times. When x1 is to be incremented to 256, overflow happens, it is
-- turned to 0, then incremented by 1. 
myForTest2 = clg ("{x0 := 3; x1 := 254; for x0 do x1 := val(x1)+1}")