{-defining the tree-}
data Tree = Empty | Node Tree Tree deriving (Show,Eq,Read)

{-Constructing all possible trees -}
gen 0 = [Empty]
gen n | n>0 = [Node x y|k<-[0..n-1], x<- gen k,  y<- gen (n-1-k)]

{-Counting leaves in the Tree -}
countLeaves Empty = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

{-Counting internal nodes in the Tree -}
totalNodes Empty = 1
totalNodes (Node left right )  = 1 + totalNodes left + totalNodes right 
getInternalNodes (Node left right) = (totalNodes (Node left right)) - (countLeaves(Node left right))


{--Proof of Induction
Observation:1: For a balanced tree, the number of leaf nodes(L) is 1 more than the number of internal nodes
Proof: 
Let L be the number of leaves in the tree.
base: A tree which has 1 leaf is a tree with root only. It has no internal nodes and the observation is true. 
Now let us assume that the claim is true for tree with L leaves. 
Step: Let T be a random tree with L+1 leaves. Choose any leaf(L) and trim it.
In order to make the Tree compressed we need to make the parent of L as a leaf. If the parent leaf cannot be made a leaf, we would have to choose the corresponding left or right child and trim the same.
Doing the above step we get a Tree T' with L leaves.
By induction: T' has at most L-1 internal nodes. 
so we can conclude that, T had L-1+1 = L internal nodes, and L+1 leaves, at most.--}