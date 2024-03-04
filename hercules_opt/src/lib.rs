pub mod ccp;
pub mod dce;
pub mod forkify;
pub mod gvn;
pub mod phi_elim;
pub mod pass;

pub use crate::ccp::*;
pub use crate::dce::*;
pub use crate::forkify::*;
pub use crate::gvn::*;
pub use crate::phi_elim::*;
pub use crate::pass::*;
