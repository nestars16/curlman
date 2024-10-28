#[derive(Debug)]
pub enum Error {
    InvalidUrl,
    Curl(curl::Error),
    NoBody,
    ParserError,
    Io(std::io::Error),
    InvalidState,
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
