use std::iter::once;

use itertools::Itertools;

use crate::{lexer::{Delimeter, Span, Token, TokenKind}, parser::blocks::{Block, FromVecStream, block_stream::BlockStream, block_tokens::BlockTokenStream, from_vec_stream}, utils::{Result, ThrowablePosition}};


pub struct BlockCollector {
    stream: FromVecStream,
}

impl From<Vec<Token>> for BlockCollector {
    fn from(tokens: Vec<Token>) -> Self {
        BlockCollector {
            stream: from_vec_stream(tokens),
        }
    }
}

impl BlockCollector {
    fn as_block_token_stream(&mut self) -> BlockTokenStream<&mut FromVecStream> {
        BlockTokenStream::from(&mut self.stream)
    }

    pub fn collect_block(&mut self) -> Result<Option<Block>> {
        let token = self.stream.next().transpose()?;
        if token.is_none() {
            return Ok(None);
        }
        let token = token.unwrap();

        let block = match token.kind {
            TokenKind::Tag(tag) => self.collect_tagged_block(tag.to_owned(), token.span)?,
            TokenKind::Ident(tag) => self.collect_tagged_block(tag.to_owned(), token.span)?,
            TokenKind::Dot => self.collect_tagged_block("dot".into(), token.span)?,
            TokenKind::Literal(_) => Block::anonymous_block(vec![token]),
            TokenKind::Group(Delimeter::Parens, tokens) => Block::anonymous_block(tokens),
            TokenKind::Group(_, _) => Block::anonymous_block(vec![token]),
            token_kind => return token.span.syntax(format!("Expected identifier: found {:?}", token_kind)),
        };
        Ok(Some(block))
    }

    fn collect_tagged_block(&mut self, tag: String, tag_span: Span) -> Result<Block> {
        let (header, header_token) = self.collect_block_header()?;
        let (mut body, chain) = if let Some(Token{ kind:TokenKind::Eq, .. }) = header_token {
            let tokens = self.as_block_token_stream().collect_block_tokens()?.unwrap_or(vec![]);
            (tokens, None)
        } else {
            let (body, chain) = self.collect_body_and_chain(header_token)?;
            let chain = chain.and_then(|tokens| {
                let block = BlockStream::from(tokens).next()?;
                Some(block.map(Box::new))
            }).transpose()?;
            (body, chain)
        };

        if body.len() == 1 && let Some(Token { kind: TokenKind::Semicolon, .. }) = body.last() {
            body.clear();
        }
        
        let span = match (&header[..], &body[..]) {
            ([..], [.., last]) => tag_span.union(&last.span),
            ([.., last], []) => tag_span.union(&last.span),
            ([], []) => tag_span,
        };
        assert!(self.stream.next().is_none());
        Ok(Block { tag, span, header, body, chain })
    }

    fn collect_block_header(&mut self) -> Result<(Vec<Token>, Option<Token>)> {
        let tokens = self.stream
            .peeking_take_while(|token| match token {
                Ok(Token{kind, ..}) => !matches!(
                    kind,
                    TokenKind::Colon | TokenKind::Eq | TokenKind::Arrow2 | TokenKind::Semicolon | TokenKind::Group(Delimeter::Braces, _) | TokenKind::NewLine
                ),
                Err(_) => true,
            })
            .try_collect()?;

        if let Some(Ok(Token{kind:TokenKind::Colon | TokenKind::Eq, ..})) = self.stream.peek() {
            Ok((tokens, self.stream.next().transpose()?))
        } else {
            Ok((tokens, None))
        }
    }

    fn collect_body_and_chain(&mut self, header_token: Option<Token>) -> Result<(Vec<Token>, Option<Vec<Token>>)> {
        let tokens: Vec<Token> = self.stream
            .peeking_take_while(|token| match token {
                Ok(Token{kind, ..}) => !matches!(
                    kind,
                    TokenKind::Semicolon | TokenKind::Arrow2 | TokenKind::Group(Delimeter::Braces, _) | TokenKind::NewLine
                ),
                Err(_) => false,
            })
            .try_collect()?;

        let end_token = match self.stream.next() {
            None => return Ok((tokens, None)),
            Some(Err(e)) => return Err(e),
            Some(Ok(token)) => token,
        };

        let tokens = match &end_token.kind {
            TokenKind::Semicolon => return Ok((tokens.into_iter().chain(once(end_token)).collect(), None)),
            TokenKind::NewLine | TokenKind::Arrow2 => {
                tokens
            }
            TokenKind::Group(Delimeter::Braces, group_tokens) => {
                if header_token.is_some() && !tokens.is_empty() {
                    let mut tokens = tokens;
                    tokens.push(end_token);
                    tokens
                } else {
                    assert!(tokens.is_empty());
                    group_tokens.clone()
                }
            }
            _ => return end_token.span.syntax("Unexpected end of block".to_owned()),
        };

        // check for chaining
        match self.stream.peek() {
            None | Some(Ok(Token{kind:TokenKind::NewLine, ..})) => return Ok((tokens, None)),
            Some(Err(e)) => return Err(e.clone()),
            Some(Ok(_)) => (),
        };

        let chained_tokens = self.as_block_token_stream().collect_block_tokens()?;
        Ok((tokens, chained_tokens))
    }
}
