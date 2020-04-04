{-# LANGUAGE OverloadedStrings #-} -- this lets the compiler turn string literals into Texts
module ParserA
    ()
where

import           Data.Char                      ( isAlphaNum )
import           Control.Monad                  ( liftM )

import           Control.Monad.State.Lazy       ( StateT(..)
                                                , evalState
                                                , get
                                                , put
                                                )
import           Control.Monad.Except           ( ExceptT(..)
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.Identity

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T

import           Dom

newtype Parser = Parser T.Text

type ParserS = ExceptT T.Text (StateT Parser Identity)

runParserS p s = evalState (runExceptT p) s -- also takes s

nextchr :: Parser -> T.Text -> Char
nextchr (Parser input) s = T.head s -- also takes s

startsWith :: Parser -> T.Text -> Bool
startsWith (Parser input) s = s `T.isPrefixOf` input

eof :: Parser -> Bool
eof (Parser input) = T.null input

consumeChar :: ParserS Char
consumeChar = do
    (Parser inp) <- get
    case T.uncons inp of
        Nothing        -> throwError "ERROR: unexpectedly reached end of file"
        Just (c, inp') -> do
            put (Parser inp')
            return c

consumeWhile :: (Char -> Bool) -> ParserS T.Text
consumeWhile f = do
    Parser input <- get
    let (s, input') = T.span f input
    put $ Parser input'
    return s

consumeWhitespace :: ParserS T.Text
consumeWhitespace = consumeWhile (== ' ')

assert :: T.Text -> Bool -> ParserS ()
assert s b = if b then return () else throwError s

parseTagName :: ParserS T.Text
parseTagName = consumeWhile isAlphaNum

parseText :: ParserS Node
parseText = text <$> consumeWhile (/= '<')
-- parseText = liftM text $ consumeWhile (/= '<')

parseNode :: ParserS Node
parseNode = do
    p <- get
    if nextchr p == '<' then parseElement else parseText


parseElement :: ParserS Node
parseElement = do
    -- open tag
    consumeChar >>= assert "missing < in open tag" . (== '<')
    tag   <- parseTagName
    attrs <- parseAttributes
    consumeChar >>= assert "missing > in open tag" . (== '>')
    -- contents
    children <- parseNodes
    -- end tag
    consumeChar >>= assert "missing < in close tag" . (== '<')
    consumeChar >>= assert "missing / in close tag" . (== '/')
    parseTagName >>= assert "end tag doesn't match start tag" . (== tag)
    consumeChar >>= assert "missing > in close tag" . (== '>')

    return $ Dom.elem tag attrs children

parseAttr :: ParserS (T.Text, T.Text)
parseAttr = do
    name <- parseTagName
    consumeChar >>= assert "missing =" . (== '=')
    value <- parseAttrValue
    return (name, value)

parseAttrValue :: ParserS T.Text
parseAttrValue = do
    open <- consumeChar
    assert "invalid open" (open == '\"' || open == '\'')
    val <- consumeWhile (/= open)
    consumeChar >>= assert "invalid close" . (== open)
    return val

parseAttributes :: ParserS AttrMap
parseAttributes = parseAttributes' HM.empty
  where
    parseAttributes' attrs = do
        consumeWhitespace
        p <- get
        if nextchr p == '>'
            then return attrs
            else do
                (name, val) <- parseAttr
                parseAttributes' $ HM.insert name val attrs

parseNodes :: ParserS [Node]
parseNodes = parseNodes' []
  where
    parseNodes' nodes = do
        consumeWhitespace
        p <- get
        if eof p || p `startsWith` "</"
            then return nodes
            else parseNode >>= parseNodes' . (nodes ++) . (: []) -- slow for big DOM










