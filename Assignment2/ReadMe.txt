Sample Input/Output

Case1: skk = i 
Input:
*Main> ev (As (As (Ls 0 (Ls 1 (Ls 2 (As (As (Vs 0) (Vs 2)) (As (Vs 1) (Vs 2)))))) (Ls 0 (Ls 1 (Vs 0)))) (Ls 0 (Ls 1 (Vs 0))))
Output:
Ls 2 (Vs 0)

Case 2: reduction of Two 
Input:
*Main> ev (Ls 0 (Ls 1 (As (Vs 0) (As (Ls 2 (As (Vs 0) (Vs 2))) (Vs 1)))))
Output:
Ls 0 (Ls 1 (As (Vs 0) (As (Vs 2) (Vs 1))))

