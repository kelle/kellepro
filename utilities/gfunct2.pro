pro gfunct2, X, A, F ;, pder

bx = EXP(A[1] * X) ; where A[1] = -1/T
F = A[0] * bx + A[2]

;IF n_params() ge 4 then pder = [[bx], [A[0] * X * bx],[replicate(1.0,n_elements(X))]]

END
