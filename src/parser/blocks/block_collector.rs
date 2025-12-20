use std::iter::once;

use itertools::Itertools;

use crate::{lexer::{Delimeter, Span, Token, TokenKind}, parser::blocks::{Block, BlockTag, FromVecStream, block_stream::BlockStream, block_tokens::BlockTokenStream, from_vec_stream}, utils::{Result, ThrowablePosition}};


fn is_block_end_token(token: &TokenKind, collect_operators: bool) -> bool {
    matches!(token,
        TokenKind::Semicolon | TokenKind::Arrow2 | 
        TokenKind::Group(Delimeter::Braces, _) | TokenKind::NewLine
    ) || (collect_operators && token.is_operator())
}

pub struct BlockCollector {
    stream: FromVecStream,
    collect_operators: bool,
}

impl From<Vec<Token>> for BlockCollector {
    fn from(tokens: Vec<Token>) -> Self {
        BlockCollector {
            stream: from_vec_stream(tokens),
            collect_operators: false,
        }
    }
}

impl BlockCollector {
    pub fn collect_operators(mut self, collect_operators: bool) -> Self {
        self.collect_operators = collect_operators;
        self
    }

    fn as_block_token_stream(&mut self) -> BlockTokenStream<&mut FromVecStream> {
        BlockTokenStream::from(&mut self.stream)
    }

    pub fn collect_block(&mut self) -> Result<Option<Block>> {
        let Some(token) = self.stream.next().transpose()? else {
            return Ok(None);
        };

        let peeked_is_operator_or_none = if let Some(Ok(peeked)) = self.stream.peek() {
            self.collect_operators && peeked.kind.is_operator()
        } else { true };

        let tag = BlockTag::new(token.kind.clone());
        let block = match token.kind {
            TokenKind::Underscore if peeked_is_operator_or_none => self.collect_anonymous_block(vec![token])?,
            TokenKind::Ident(_) if peeked_is_operator_or_none => self.collect_anonymous_block(vec![token])?,
            TokenKind::Tag(_) if peeked_is_operator_or_none => self.collect_anonymous_block(vec![token])?,
            TokenKind::Literal(_) => self.collect_anonymous_block(vec![token])?,
            TokenKind::Group(Delimeter::Parens, tokens) => self.collect_anonymous_block(tokens)?,
            TokenKind::Group(_, _) => self.collect_anonymous_block(vec![token])?,
            _ if tag.is_some() => self.collect_tagged_block(tag.unwrap(), token.span)?,
            token_kind => return token.span.syntax(format!("Expected identifier: found {:?}", token_kind)),
        };
        Ok(Some(block))
    }

    fn collect_anonymous_block(&mut self, header: Vec<Token>) -> Result<Block> {
        assert!(!header.is_empty());
        let tag = BlockTag::Anonymous;
        let span = header[0].span.clone();
        let (mut body, chain) = self.collect_body_and_chain(None)?;
        if body.len() == 1 && let Some(Token { kind: TokenKind::Semicolon, .. }) = body.last() {
            body.clear();
        }
        assert!(body.is_empty());
        let chain = chain.and_then(|tokens| {
            let block = BlockStream::from(tokens).collect_operators(self.collect_operators).next()?;
            Some(block.map(Box::new))
        }).transpose()?;
        assert!(self.stream.next().is_none());
        Ok(Block { tag, span, header, body, chain })
    }

    fn collect_tagged_block(&mut self, tag: BlockTag, tag_span: Span) -> Result<Block> {
        if let BlockTag::Ident(_) = &tag {
            self.collect_operators = false;
        }
        let (header, header_token) = self.collect_block_header()?;
        let (mut body, chain) = if let Some(Token{ kind:TokenKind::Eq, .. }) = header_token.clone() {
            let tokens = self.as_block_token_stream().collect_block_tokens()?.unwrap_or(vec![]);
            (tokens, None)
        } else {
            let (body, chain) = self.collect_body_and_chain(header_token.clone())?;
            let chain = chain.and_then(|tokens| {
                let block = BlockStream::from(tokens).collect_operators(self.collect_operators).next()?;
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
            ([], []) => tag_span.clone(),
        };
        assert!(self.stream.next().is_none());
        Ok(Block { tag, span, header, body, chain })
    }

    fn collect_block_header(&mut self) -> Result<(Vec<Token>, Option<Token>)> {
        let tokens = self.stream
            .peeking_take_while(|token| match token {
                Ok(Token{kind, ..}) => !matches!(kind, TokenKind::Colon | TokenKind::Eq)
                                                    && !is_block_end_token(kind, self.collect_operators),
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
                Ok(Token{kind, ..}) => !is_block_end_token(kind, self.collect_operators),
                Err(_) => false,
            })
            .try_collect()?;

        let end_token = match self.stream.peek() {
            Some(Ok(token @ Token { kind, .. })) if kind.is_operator() => token.clone(),
            _ => match self.stream.next() {
                None => return Ok((tokens, None)),
                Some(Err(e)) => return Err(e),
                Some(Ok(token)) => token,
            }
        };

        let tokens = match &end_token.kind {
            TokenKind::Semicolon => return Ok((tokens.into_iter().chain(once(end_token)).collect(), None)),
            TokenKind::NewLine | TokenKind::Arrow2 => tokens,
            token_kind if token_kind.is_operator() => tokens,
            TokenKind::Group(Delimeter::Braces, group_tokens) => {
                if header_token.is_some() && !tokens.is_empty() {
                    tokens.into_iter().chain(once(end_token)).collect()
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
