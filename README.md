# Programming theory tasks

1. [task 1](./task1.diff)
    ```diff
    --- a/urm/engine.py
    +++ b/urm/engine.py
    @@ -40,7 +40,7 @@ def run(prgm, *args):
         Returns
             result of running if the program 'prgm' halts
         """
    -    mem = list(args)
    +    mem = [0, *args]
         ic, lprgm = 1, len(prgm)
         while 1 <= ic <= lprgm:
             stm = prgm[ic - 1]
    ```
2. task 2:
    - [superposition](./superposition.py)  
    
    To run: `python3 superposition.py n g f1 f2 f3...`
    Example: `python3 superposition.py 2 ./programs/sum.urm ./programs/sum.urm ./programs/sum.urm > ./superposition/sup.urm`
    - [primitive recursion](./recursion.py)  
    
    To run: `python3 recursion.py n f g`
    Example: `python3 recursion.py 1 ./recursion/f.urm ./recursion/g.urm > ./recursion/rec.urm`
    - [minimization](./minimization.py)  
    
    To run: `python3 minimization.py n f`  
    Example: `python3 minimization.py 1 ./minimization/f.urm > ./minimization/min.urm`
    where `n` is number of function arguments, `f`, `g`, `f1`, ..., `fn` paths to function files.
