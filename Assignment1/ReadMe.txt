Three programs have been written for the assignment

1. For generating all possible trees
Sample Input/Out:
*Main> gen 3
[Node Empty (Node Empty (Node Empty Empty)),Node Empty (Node (Node Empty Empty) Empty),Node (Node Empty Empty) (Node Empty Empty),Node (Node Empty (Node Empty Empty)) Empty,Node (Node (Node Empty Empty) Empty) Empty]
*Main> mapM_ print(gen 3)
Node Empty (Node Empty (Node Empty Empty))
Node Empty (Node (Node Empty Empty) Empty)
Node (Node Empty Empty) (Node Empty Empty)
Node (Node Empty (Node Empty Empty)) Empty
Node (Node (Node Empty Empty) Empty) Empty

2. For Counting all Leaves in the Tree
Sample Input/Out:
*Main> countLeaves (Node Empty (Node Empty (Node Empty Empty)))
4


3. For Counting all Internal Nodes in the Tree
Sample Input/Out:
*Main> getInternalNodes (Node Empty (Node Empty (Node Empty Empty)))
3

