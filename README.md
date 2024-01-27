# c-codegen-si
A C source code generation tool for analyzing scalar interpolation optimization pass in LLVM.

# Config:
Here's the description of the parameters that are passed to the fuzzer. ("--" is used to denote a comment)
```json
{
  "knobs": {
    -- The number of maximum dimensions
    "maxDims":  <int>,

    -- The number of singleton variables that are generated: (min, max)
    "noOfSingletonRange": [<int>, <int>],

    -- The number of array variables that are generated: (min, max)
    "noOfArrayRange": [<int>, <int>],

    -- The size of any dimension of an array that are known ahead of time: (min, max)
    "sizeRange": [<int>, <int>],

    -- The number of assignment statements inside the target (vectorizable) loop: (min, max)
    "loopDepthRange": [<int>, <int>],

    -- The number of loop nesting: (min, max)
    "nestedLoopRange": [<int>, <int>],

    -- The number of total loop blocks (excluding nestings): (min, max)
    "noLoopRange": [<int>, <int>],

    -- The stride of each induction variable of a loop: (min, max)
    "strideRange": [<int>, <int>],

    -- The number of expressions in the right hand side of an assignment statement: (min, max)
    "expressionDepthRange": [<int>, <int>],

    -- Tunes the probability for accessing the outermost dimensions of an array: positive integer
    "weightCoeffForDims": <int>,

    -- The number of total functions generated
    "noOfFunctions": <int>,

    -- All the arithmetic types that will be included in the program: subset of ["int", "uint", "long", "ulong", "char"],
    "targetDTypes": ["string"(,"string")*?],

    -- Use modulo to ensure in-bound array access
    "useModsInOuterLoop": bool,

    -- Number of times an outer loop statement is repeated
    "repeatFactor": <int>,
    
    -- The minimum time the loops should be executed
    "timeLimit": <float>

    -- Allow reduction operation
    "allowReduction": bool
  }
}
```
