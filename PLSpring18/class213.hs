
-- operationsal semantics for lambda calculus
-- use lambda calc to compare different evaluation stratigies

-- everything in lambda calc is a function
-- redex or reducible expression is an expression that can be beta reduced

--semantics:
--beta reduction
--alpha reduction
--eta reduction

--what is divergence??
--strong normalization is when every expression reduces to a unique value
-- lambda calc does not satisfy this property (it has inifinte loops)

-- (\ x . x) (\ x . x)
-- example of a diverging expresssion (this is smallest possible)

-- how does eta reduction compare with dynamic scoping??

-- rules are nondeterministic, but actual implementation needs to define
-- an evaluation strategy, what reductions get processed when.

-- call-by-value semantics
-- only the top most function that is applied to a value is reduced
-- i.e. arguments are evaluated first

-- call-by-name semantics
-- can do any reduction as long as the left hand expresssion is a value

-- are these in some  sense the difference between post and pre order tree traversal

-- call-by-name by its self is inefficient because you have to pass stuff around

-- call-by-need or lazy evaluation is a modification of call-by-name
-- track occurances coming from the same argument
-- when forced to reduce one, reduce others
-- you 'need' somehting when you try to evaluate a value into a function body

-- 'need' in haskell is pattern matching
-- when you go to evaluate a match statement
-- evaluation takes place step by step
