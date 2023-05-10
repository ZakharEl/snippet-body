//! Library to allow the representation and compartmentalization of snippet bodies into segment like those found in [textmate editor](https://macromates.com/manual/en/snippets) and [visual studio code](https://code.visualstudio.com/docs/editor/userdefinedsnippets).
//! Does not actually choose what segment type are to be supported.
//! That is the responsability of another program that implements this library.
//! This is allow this library to be unopininionated about what functionality to include and exclude as well as adhere to the unix philosophy of doing 1 thing and doing 1 well.
#[cfg(not(feature = "json-debug"))]
mod standard_body;
#[cfg(not(feature = "json-debug"))]
pub use standard_body::*;

#[cfg(feature = "json-debug")]
mod serde_body;
#[cfg(feature = "json-debug")]
pub use serde_body::*;
