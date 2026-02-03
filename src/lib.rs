#![recursion_limit = "512"]
// Compiler functions naturally accumulate parameters (context, types, spans, flags).
// Refactoring every one into a struct would add boilerplate without improving clarity.
#![allow(clippy::too_many_arguments)]
// Complex return types arise naturally in compiler data structures; type aliases
// would just move the complexity elsewhere.
#![allow(clippy::type_complexity)]
// Peephole passes use index-based iteration over instruction arrays where the loop
// variable is used as both an index and for bounds arithmetic. Converting to iterators
// would obscure the sliding-window logic.
#![allow(clippy::needless_range_loop)]

pub(crate) mod common;
pub(crate) mod frontend;
pub(crate) mod ir;
pub(crate) mod passes;
pub mod backend;
pub mod driver;
