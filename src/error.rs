#[derive(Debug)]
pub enum Error {
    InvalidUrl,
    Curl(curl::Error),
    NoBody,
    ParserError,
}

impl From<curl::Error> for Error {
    fn from(value: curl::Error) -> Self {
        Self::Curl(value)
    }
}
