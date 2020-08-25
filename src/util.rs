use std::fmt::{Display, Formatter, Result};

pub enum RuntimeException {
    NoMainFunction
}

impl Display for RuntimeException {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        return match self {
            RuntimeException::NoMainFunction => write!(f, "NoMainFunctionException: Unable to find main function."),
        }
    }
}

