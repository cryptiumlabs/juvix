#### Context creation

Put all definitions in a context for lookup.

#### Datatype conversion

Convert frontend representation of datatypes & pattern-matching into core representation of datatypes & pattern-matching.

#### Signature combination

Combine signature of function & function definition into annotated term.

#### De-sugaring

##### `do` operator

Turn `do` operator into sequence of monadic operations as appropriate.

##### Modules

Turn modules into records.

##### Records

Turn record syntax into a regular datatype, de-alias records.

##### `if` statements

Desugar `if` statements into match/case.

##### Guard statements

Desugar guard statements into match/case.

##### Explicitize arguments

Make implicit arguments explicit.

##### De-aliasing

De-alias named types, newtype declarations, type aliases.

#### Backend-specific

##### Constant conversion

Convert numbers & strings to backend-specific representation.

##### Datatype compilation

Compile datatypes & pattern-matching to backend-specific operations.
