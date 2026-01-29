#[derive(Debug)]
pub enum Error {
    UnsupportedMethod(String),
    MissingUrl(&'static str),
    UnknownVar(String),
    Curl(curl::Error),
    NoBody,
    Io(std::io::Error),
    InvalidState(&'static str),
}

pub mod parser {
    #[derive(Debug, Clone, PartialEq)]
    pub struct Error {
        pub kind: ErrorKind,
        pub stage: ErrorStage,
        pub line: usize,
        pub column: usize,
        pub message: String,
        pub context: Vec<String>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ErrorStage {
        Lexing,
        Ir,
        Parsing,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ErrorKind {
        MissingUrl,
        InvalidFlag(String),
        InvalidUrl(String),
        InvalidFlagValue(String),
        InvalidRequest(String),
    }

    impl Error {
        pub fn new(
            kind: ErrorKind,
            stage: ErrorStage,
            line: usize,
            column: usize,
            message: impl Into<String>,
            context: Vec<String>,
        ) -> Self {
            Self {
                kind,
                stage,
                line,
                column,
                message: message.into(),
                context,
            }
        }
    }
}

impl ToString for Error {
    fn to_string(&self) -> String {
        match self {
            Error::UnsupportedMethod(method) => {
                format!("Unsupported Method:\n{method}")
            }
            Error::MissingUrl(text) => text.to_string(),
            Error::Curl(error) => {
                format!("cURL error:\n{}", error.to_string())
            }
            Error::NoBody => "Request is missing body".to_string(),
            Error::Io(error) => {
                format!("I/O error:\n{}", error.to_string())
            }
            Error::InvalidState(reason) => {
                format!("Error:\n{reason}")
            }
            Error::UnknownVar(var) => {
                format!("Unset Variable:\n{var}")
            }
        }
    }
}

impl From<curl::Error> for Error {
    fn from(value: curl::Error) -> Self {
        Self::Curl(value)
    }
}
impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}
