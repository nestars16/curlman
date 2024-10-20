use curl::easy::Easy;

use crate::{error::Error, types::RequestInfo};

fn perform(req: RequestInfo) -> Result<(), Error> {
    let mut handle = Easy::new();

    let Some(url) = req.url else {
        return Err(Error::InvalidUrl);
    };

    handle.url(&url.to_string()).unwrap();
    Ok(())
}
