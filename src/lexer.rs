mod no_comments;
mod tokens;
mod blocks;

pub use tokens::{Delimeter, Token, TokenStream, Value};
pub use blocks::{Block, BlockStream};

pub fn blockify(source: &str) -> BlockStream {
    BlockStream::from(source)
}