-- Written by: Pedro de Oliveira Lira
--             pdeolive@syr.edu

import Test.QuickCheck
import System.Process(system)
import System.IO( hPutStrLn, withFile, IOMode(WriteMode) )
import Control.Monad( liftM, liftM2)
import Data.List
import Data.Char
import Text.Printf hiding (toChar)



------------------------------------------------------------------------
-- Binary trees
data BTree = Empty | Branch Char BTree BTree
            deriving (Show,Eq)

-- Multiway trees
data MTree = Node Char [MTree]
        deriving (Eq, Show)

------------------------------------------------------------------------
-- Examples 

t1 = Branch 'm' 
       (Branch 't' 
         (Branch 'a' Empty Empty) 
         Empty) 
       (Branch 'q' 
         (Branch 'm' Empty Empty) 
         (Branch 'w' Empty Empty))

t2 = Node 'u' 
       [Node 'c' [],
        Node 'q' [],
        Node 'n' 
          [Node 'm' [],
           Node 'g' [],
           Node 'j' []],
        Node 'y' 
          [Node 'z' []]]


-- Counting BTree Branch nodes
bcount :: BTree -> Int
bcount Empty = 0
bcount (Branch _ tl tr) = 1 + bcount tl + bcount tr

-- Counting MTree Nodes
mcount :: MTree -> Int
mcount (Node _ ts) = 1 + sum (map mcount ts)

--  preorder traversal for BTrees
preorder Empty = ""
preorder (Branch c tl tr) = c:(preorder tl ++ preorder tr)

-- inorder traversal for BTrees
inorder Empty = ""
inorder (Branch c tl tr) = inorder tl ++ [c] ++ inorder tr
------------------------------------------------------------------------
------------------------------------------------------------------------
fix = error "Please fix me!"
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 1: BTree depth
bmaxDepth ::  BTree -> Int 
bmaxDepth Empty = -1 
bmaxDepth (Branch _ tl tr) = 1 + max (bmaxDepth tl) (bmaxDepth tr) 
------------------------------------------------------------------------
-- Problem 2: MTree depth
mmaxDepth (Node _ []) = 0
mmaxDepth (Node _ ns) = 1 + getMaxList [mmaxDepth n | n <- ns]

-- helper function to get the maximum int
-- from a list of ints
getMaxList :: [Int] -> Int
getMaxList [] = 0
getMaxList [x] = x
getMaxList (fst:snd:rst) = max (max (fst) (snd)) (getMaxList rst)


------------------------------------------------------------------------
-- Problem 3: Collecting BTree leaves
bleaves :: BTree -> String
bleaves Empty = ""
bleaves (Branch l tl tr) = if tl == Empty && tr == Empty
                           then [l] ++ bleaves tl ++ bleaves tr
                           else bleaves tl ++ bleaves tr

------------------------------------------------------------------------
-- Problem 4: Collecting MTree leaves
mleaves :: MTree -> String
mleaves (Node l []) = [l] 
mleaves (Node l ns) =  concat [mleaves n | n <- ns]

------------------------------------------------------------------------
-- Problem 5: BTree levels
blevel :: Int -> BTree -> String
blevel 0 _ = ""
blevel i (Empty) = ""
blevel 1 (Branch l _ _) = [l]
blevel i (Branch l tl tr) = blevel (i-1) (tl) ++ blevel (i-1) (tr)
                           

------------------------------------------------------------------------
-- Problem 6: MTree levels 
mlevel :: Int -> MTree -> String
mlevel 0 _ = ""
mlevel 1 (Node l _) = [l]
mlevel i (Node l ns) = concat [mlevel (i-1) n | n <- ns]

------------------------------------------------------------------------
-- Problem 7: Postfix
postfix Empty = ""
postfix (Branch l tl tr) = postfix tl ++ postfix tr ++ [l]

------------------------------------------------------------------------
-- Problem 8: reconstructing a BTree from its traversals
reconstruct :: String -> String -> BTree
reconstruct "" "" = Empty

------------------------------------------------------------------------
-- Problem 9: making BTrees
makeTrees 0 = [Empty]
makeTrees 1 = [Branch 'x' Empty Empty] 

------------------------------------------------------------------------
-- drawing
------------------------------------------------------------------------
-- The following assumes Mac OS X with graphviz installed.
-- (See the download section of: http://www.graphviz.org.)
-- There are probably easy patches to make this work on 
-- other OS's since dump is the only system dependent function.

-- dump str 
--   writes the temp file /tmp/graph.gv with contents str
--   then opens it

dump str = do { withFile "/tmp/graph.gv" WriteMode action
              ; system "open /tmp/graph.gv"
              }
    where action handle = hPutStrLn handle str 

-- (drawBTree t) creates a gv description of t and displays it
drawBTree t = dump $ start  ++ nodes ++ edges ++ end
    where
      start = "digraph g {\n    "
      (nodes,edges) = draw t 1
      end   = "}\n"

-- draw tree root_address = (node_decls, edge_decls)
draw :: BTree -> Integer -> (String,String)
draw Empty m = (inode m,"")
    where inode m = (show m) ++ " [style=invis];\n    "
draw (Branch c tl tr) m 
    = ((node c m)++nl++nr,(edge m ml tl)++(edge m mr tr)++el++er)
    where ml      = 2*m
          mr      = 2*m+1
          (nl,el) = draw tl ml
          (nr,er) = draw tr mr
          node c m      = (show m) ++ " [label=" ++ (show [c]) ++ "];\n    "
          edge m n Empty = (show m) ++ "->" ++ (show n) 
                          ++" [style=invis];\n    "
          edge m n _    = (show m) ++ "->" ++ (show n) ++ ";\n    "


-- (drawMTree t) creates a gv description of t and displays it
drawMTree t = dump $ start  ++ nodes ++ edges ++ end
    where
      start = "digraph g {\n    "
      (nodes,edges) = mdrawAux (t,"X")
      end   = "}\n"


-- draw tree root_address = (node_decls, edge_decls)
mdraw :: MTree -> (String,String)
mdraw mt = mdrawAux (mt,"X")

mdrawAux :: (MTree,String) -> (String,String)
mdrawAux (Node c ts,tag) = (nodes,edges)
    where 
      taggedts = zip ts (map (\k->tag++'x':show k) [0..])
      subs = map mdrawAux taggedts
      edge (Node _ _,tag') = tag ++ " -> " ++ tag' ++            ";\n    "
      nodes = (tag ++ " [label=" ++ (show [c]) ++ "];\n    ")
              ++ concatMap fst subs
      edges = (concatMap edge taggedts) ++ concatMap snd subs
------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------

t10 = Branch 'm' 
     (Branch 'h' 
      (Branch 'c' 
       (Branch 'a' Empty Empty) 
       (Branch 'e' Empty (Branch 'f' Empty Empty)))
      Empty)
     (Branch 'u' 
      (Branch 's' (Branch 'p' Empty Empty) Empty)
      (Branch 'z' Empty Empty))

t13 = Branch 'm'
     (Branch 't' Empty Empty)
     (Branch 'x' Empty
             (Branch 'j' Empty Empty))
             
t0 = Branch 'r' Empty Empty

t3 = Empty


t02 = Node 'x' []

-----------------------------------------------------------------------
-- Function to call all my tests
-- It returns true if the test is sucessfull. 

tests :: [(String, Bool)]
tests = [ ("bmaxDepth_test1",     bmaxDepth_test1)
        , ("bmaxDepth_test2",       bmaxDepth_test2)  
        , ("bmaxDepth_test3",  bmaxDepth_test3)  
        , ("mmaxDepth_test1",     mmaxDepth_test1)  
        , ("mmaxDepth_test2",     mmaxDepth_test2)  
        , ("blevel_test1",       blevel_test1)  
        , ("blevel_test2",       blevel_test2)  
        , ("blevel_test3",       blevel_test3)
        , ("blevel_test4",       blevel_test4)
        , ("mlevel_test1",       mlevel_test1)  
        , ("mlevel_test2",       mlevel_test2)  
        , ("mlevel_test3",       mlevel_test3)
        , ("mlevel_test4",       mlevel_test4)
        ]


-----------------------------------------------------------------------
-- Testing for bmaxDepth

bmaxDepth_test1 = bmaxDepth t13 == 2
bmaxDepth_test2 = bmaxDepth t0 == 0
bmaxDepth_test3 = bmaxDepth t3 == -1

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Testing for mmaxDepth

mmaxDepth_test1 = mmaxDepth t2 == 2
mmaxDepth_test2 = mmaxDepth t02 == 0

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Testing for blevel

blevel_test1 = blevel 0 t13 == ""
blevel_test2 = blevel 1241 t13 == ""
blevel_test3 = blevel 3 Empty == ""
blevel_test4 = blevel 1 t0 == "r"

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Testing for mlevel

mlevel_test1 = mlevel 0 t2 == ""
mlevel_test2 = mlevel 1241 t2 == ""
mlevel_test3 = mlevel 1 t02 == "x"
mlevel_test4 = mlevel 3 t2 == "mgjz"

-----------------------------------------------------------------------

-- QuickCheck BTree generator
instance Arbitrary BTree where
    arbitrary = sized tree
        where
          tree 0 = return Empty
          tree n = do c <- elements (['a'..'z']++['A'..'Z'])
                      m1 <- elements [0..(2*n `div` 3)]
                      m2 <- elements [0..(2*n `div` 3)]
                      liftM2 (Branch c) (variant 1 (tree m1))
                                        (variant 2 (tree m2))

-- QuickCheck MTree generator
instance Arbitrary MTree where
    arbitrary = sized tree
        where
          tree n = do c <- elements (['a'..'z']++['A'..'Z'])
                      m <- elements [0..n]
                      liftM (Node c) (liftM (take m)
                                           (listOf (tree (2*(n-m)`div` 3))))



------------------------------------------------------------------------
-- Testing for bleaves

bleaves_prop t = and $ zipWith (==) (bleaves (bleafRel t)) ['a'..maxBound]

bleafRel :: BTree -> BTree
bleafRel t = fst $ relab (t,['a'..maxBound])
    where 
      relab (Empty,cs) = (Empty,cs)
      relab (Branch _ Empty Empty,c:cs)=(Branch c Empty Empty,cs)
      relab (Branch c tl tr,cs) = (Branch c tl' tr',cs'')
          where (tl',cs') = relab (tl,cs)
                (tr',cs'') = relab (tr,cs')


------------------------------------------------------------------------
-- Testing for mleaves

mleaves_prop t = and $ zipWith (==) (mleaves (mleafRel t)) ['a'..maxBound]

mleafRel :: MTree -> MTree
mleafRel t = fst $ relab (t,['a'..maxBound])
    where 
      relab (Node _ [],c:cs)=(Node c [],cs)
      relab (Node c ts,cs)  = foo (ts,[],cs)
          where foo ([],ts',cs) = (Node c (reverse ts'),cs)
                foo (t:ts,ts',cs) = let (t',cs') = relab (t,cs)
                                    in foo (ts,t':ts',cs')



------------------------------------------------------------------------
-- Testing for postfix

-- Run: quickCheck postfix_prop
postfix_prop t = (str == (take (length str) ['a'..maxBound]))
    where t' = postLabel t
          str = reverse (postfix t')
          
postLabel t = fst (relab t ['a'..maxBound])
    where
      relab Empty cs = (Empty,cs)
      relab (Branch _ tl tr) (c:cs) = (Branch c tl' tr',cs'')
          where (tr',cs')  = relab tr cs
                (tl',cs'') = relab tl cs'

------------------------------------------------------------------------
-- Testing for reconstruct

makeUniq :: BTree -> BTree
makeUniq t = fst (step t chars)
    where chars = [(minBound::Char)..(maxBound::Char)]
          step Empty cs = (Empty,cs)
          step (Branch _ tl tr) (c:cs) = (Branch c tl' tr',cs'')
              where (tl',cs') = step tl cs
                    (tr',cs'') = step tr cs'

recon_prop t = (t' == (reconstruct (preorder t') (inorder t')))
    where t' = makeUniq t

------------------------------------------------------------------------
-- Testing for makeTrees

makeTreesTest = all test [0..8]
    where
      test n = let trees = makeTrees n
               in (all (\t->bcount t == n) trees)
                  && ( length(nub trees) == cats!!n)

-- the first 20 Catalan numbers 
-- see: http://en.wikipedia.org/wiki/Catalan_number
cats = [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 
        742900, 2674440, 9694845, 35357670, 129644790, 477638700, 1767263190]
