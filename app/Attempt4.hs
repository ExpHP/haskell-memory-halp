{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Prelude hiding (sequence,sequence_)
import           "base" Debug.Trace
import           "base" Data.Functor
import           "base" Data.Foldable hiding (sequence, sequence_)
import           "base" Data.Function((&))
import qualified "base" Data.List as List
import           "base" Data.Complex
import           "base" Control.Monad hiding (sequence,sequence_)
import           "base" Control.Applicative
import           "aeson" Data.Aeson
import           "aeson" Data.Aeson.Types
import           "vector" Data.Vector(Vector)
import           "text" Data.Text(Text)
import           "yaml" Text.Libyaml(Event(..), Tag(..))
import qualified "yaml" Text.Libyaml as LibYaml
import qualified "vector" Data.Vector as Vector
import           "bytestring" Data.ByteString(ByteString)
import qualified "bytestring" Data.ByteString.Char8 as ByteString
import qualified "unordered-containers" Data.HashMap.Strict as HashMap
import           "conduit" Data.Conduit -- for... ResourceT? Okay.
import           "conduit-combinators" Conduit
import           "transformers" Control.Monad.Trans.Maybe
import           "deepseq" Control.DeepSeq

import qualified "parsers" Text.Parser.Combinators as Parser
import           "conduit-parse" Data.Conduit.Parser(ConduitParser, named)
import qualified "conduit-parse" Data.Conduit.Parser as ConduitParser

import           SharedJunk

main :: IO ()
main = doMain $
    runResourceT $
    runConduitRes $
    LibYaml.decodeFile "band.yaml"
--    .| mapM_C (liftIO . print)
--    .| takeC 10
    -- .| mapM_C (liftIO . print)
    .| (ConduitParser.runConduitParser bandYaml_ $> ())
    -- .| (ConduitParser.runConduitParser bandYaml >>= liftIO . print)
--    mapM_C ((>> pure ()) . pure)

type EventParser m a = ConduitParser Event m a

-- | Type of a parser that skips something.
type Ignore = forall m. (MonadIO m)=> EventParser m ()

-- | Type of unary functions that take a "continuation parser" and supply it
--    with a modified stream of events.
type ParserCont = forall m a. (MonadIO m)=> EventParser m a -> EventParser m a

infixl 4 <<
(<<) :: Applicative m => m a -> m b -> m a
(<<) = (<*)

bandYaml_ :: (MonadIO m)=> EventParser m ()
bandYaml_ = stream_

bandYaml :: (MonadIO m)=> EventParser m (Vector (Vector (Vector (Complex Double))))
bandYaml = named "bandYaml" $
    ket                                -- EventParser m (Vector (Complex Double))
    & limitMappingValue "eigenvector"  -- EventParser m (Vector (Complex Double))
    & vector                           -- EventParser m (Vector (Vector (Complex Double)))
    & limitMappingValue "band"         -- EventParser m (Vector (Vector (Complex Double)))
    & vector                           -- EventParser m (Vector (Vector (Vector (Complex Double))))
    & limitMappingValue "phonon"       -- EventParser m (Vector (Vector (Vector (Complex Double))))
    & bracketDocument
    & bracketStream

ket :: (MonadIO m)=> EventParser m (Vector (Complex Double))
ket = named "ket" $
    parseTupleWith2 (:+) scalarFloat scalarFloat -- EventParser m (Complex Double)
    & vector         -- each coordinate...       -- EventParser m (Vector (Complex Double))
    & vector         -- each atom...             -- EventParser m (Vector (Vector (Complex Double)))
    & (>>= (pure $!!))
    & fmap (>>= id)  -- ...as 3N coordinates     -- EventParser m (Vector (Complex Double))

vector :: (MonadIO m)=> EventParser m a -> EventParser m (Vector a)
vector p = named "vector" $
    fmap (Vector.fromList . List.reverse) $ foldl'Sequence (flip (:)) [] p

------------------------
-- The things I ultimately need for the above code to work:

-- | Scan a mapping in search for a specific key, and then run the continuation parser
--   beginning on the first event for the associated value.
--
--   The continuation parser must consume exactly the events for a single node.
--   Failure behavior is not currently specified.
limitMappingValue :: ByteString -> ParserCont
limitMappingValue = limitMappingValueImpl -- defined below

-- | Read a sequence, running an event parser on the event stream beginning at each item.
--   Take the outputs of this parser and fold them.
--
--   The input parser must consume exactly the events for a single node on each run.
--   Failure behavior is not currently specified.
--
--   This version is strict, forcing the evaluation at each step.
foldl'Sequence :: (MonadIO m)=> (b -> a -> b) -> b -> EventParser m a -> EventParser m b
foldl'Sequence op init item = bracketAnchorlessSequence (doMany init) where
    doMany b = Parser.try (doSome b) <|> doNone b
    doNone b = pure b
    doSome b = do
        Parser.notFollowedBy sequenceEnd
        limitNode item >>= \a -> doMany $! (b `op` a)

-- | Parse a sequence of two items with a function.
--
--   Each input parser must consume exactly one node.
--   Failure behavior is not currently specified.
parseTupleWith2 :: (MonadIO m)=> (a -> b -> c) -> EventParser m a -> EventParser m b -> EventParser m c
parseTupleWith2 f parseA parseB = bracketAnchorlessSequence $ f <$> parseA <*> parseB

scalarFloat :: (Monad m)=> EventParser m Double
scalarFloat = eventParse $ \case (EventScalar s _ _ _) -> mayRead . ByteString.unpack $ s
                                 _                     -> Nothing

mayRead :: (Read a)=> String -> Maybe a
mayRead = fmap fst . find ((""==) . snd). reads

------------------------
-- Grammar from pyyaml.org.
--
-- stream ::= STREAM-START document* STREAM-END
-- document ::= DOCUMENT-START node DOCUMENT-END
-- node ::= ALIAS | SCALAR | sequence | mapping
-- sequence ::= SEQUENCE-START node* SEQUENCE-END
-- mapping ::= MAPPING-START (node node)* MAPPING-END

------------------------
-- A suite of value-ignoring parsers

streamStart, streamEnd, documentStart, documentEnd, sequenceEnd, mappingEnd :: Ignore
anchorlessMappingStart, anchorlessSequenceStart :: Ignore
streamStart    = eventEq EventStreamStart
streamEnd      = eventEq EventStreamEnd
documentStart  = eventEq EventDocumentStart
documentEnd    = eventEq EventDocumentEnd
sequenceEnd    = eventEq EventSequenceEnd
mappingEnd     = eventEq EventMappingEnd
anchorlessMappingStart  = eventEq (EventMappingStart Nothing)
anchorlessSequenceStart = eventEq (EventSequenceStart Nothing)

-- underscored because we ignore anchors
sequenceStart_, mappingStart_ :: Ignore
sequenceStart_ = eventSatisfy_ $ \case (EventSequenceStart _) -> True; _ -> False
mappingStart_  = eventSatisfy_ $ \case (EventMappingStart _) -> True; _ -> False

stream_, document_, node_, sequence_, mapping_ :: Ignore
stream_   = named "boring stream"   $ bracketStream streamInner_
document_ = named "boring document" $ bracketDocument node_
node_     = named "boring node"     $ alias_ <|> scalar_ <|> sequence_ <|> mapping_
sequence_ = named "boring sequence" $ sequenceStart_ >> sequenceInner_ >> sequenceEnd
mapping_  = named "boring mapping"  $ mappingStart_ >> mappingInner_ >> mappingEnd
-- sequence_ = named "boring sequence" $ sequenceStart_ >> traceP "skipping sequence" >> sequenceInner_ >> sequenceEnd
-- mapping_  = named "boring mapping"  $ mappingStart_ >> traceP "skipping mapping" >> mappingInner_ >> mappingEnd

traceP :: (MonadIO io) => String -> io ()
traceP = liftIO . traceIO

streamInner_, sequenceInner_, mappingInner_ :: Ignore
streamInner_   = Parser.skipMany document_
sequenceInner_ = Parser.skipMany node_
mappingInner_  = Parser.skipMany (node_ >> node_)

scalar_, alias_ :: Ignore
scalar_ = eventSatisfy_ $ \case (EventScalar _ _ _ _) -> True; _ -> False
alias_  = eventSatisfy_ $ \case (EventAlias _)        -> True; _ -> False

-- NOTE: Should take Text but I don't want to deal with encodings.
--       (LibYaml appears to support utf8, utf16le, and utf16be)
scalarStringLiteral_ :: ByteString -> Ignore
-- scalarStringLiteral_ x = eventSatisfy_ $ \case (EventScalar bs _ _ _) -> trace (ByteString.unpack bs) bs == x;  _ -> False
scalarStringLiteral_ x = eventSatisfy_ $ \case (EventScalar bs _ _ _) -> bs == x;  _ -> False

-- ------------------------
-- -- Event conduits. (FML)

-- -- | EventConduit is a waste of my goddamn time.
-- --
-- -- Er, let's try that again.
-- -- EventConduit is a stopgap solution from a programmer who doesn't know what he's doing.
-- --
-- -- ...okay, one last try.
-- -- EventConduit is the 'takeC' in 'takeExactlyC inner = takeC .| inner <* sinkNull'.
-- -- Essentially, an EventConduit forwards a validated prefix of an input stream.
-- --
-- -- It either:
-- --
-- -- 1. Consumes nothing, yields nothing, and returns 'Nothing'.
-- -- 2. Consumes a non-empty prefix, yields whatever it wants (but most likely /exactly that prefix/),
-- --    and returns 'Just something' (most likely `()`).
-- --
-- -- Validation errors in the middle of possibility 2 result in the immediate termination of the program.
-- -- (luckily, we're working on the well-structured output of Libyaml, not arbitrary user input)
-- --
-- -- A whole lotta stuff implemented on EventParsers needs to be reimplemented on
-- -- EventConduits as well, but without all the nice abstractions of applicatives and alternatives.
-- -- Frankly, they blow chunks, but they're the best I could think of.
-- type EventConduit m = MaybeT (ConduitM Event Event m) ()

-- takeStreamStart, takeStreamEnd, takeDocumentStart, takeDocumentEnd :: (Monad m)=> EventConduit m
-- takeSequenceStart, takeSequenceEnd, takeMappingStart, takeMappingEnd :: (Monad m)=> EventConduit m
-- takeStreamStart   = takeEventEq EventStreamStart
-- takeStreamEnd     = takeEventEq EventStreamEnd
-- takeDocumentStart = takeEventEq EventDocumentStart
-- takeDocumentEnd   = takeEventEq EventDocumentEnd
-- takeSequenceEnd   = takeEventEq EventSequenceEnd
-- takeMappingEnd    = takeEventEq EventMappingEnd
-- takeSequenceStart = takeEventSatisfy $ \case (EventSequenceStart _) -> True; _ -> False
-- takeMappingStart  = takeEventSatisfy $ \case (EventMappingStart  _) -> True; _ -> False

-- bracketTakeMany :: (Monad m)=> String -> EventConduit m -> EventConduit m -> EventConduit m -> EventConduit m
-- bracketTakeMany msg begin end item = do
--     begin
--     () <$ many item
--     end <|> pure (error msg)

-- -- | skipMany from 'Parsing', specialized to EventConduit.
-- takeMany p = () <$ many p

--   -- where
--   --   conduit = fmap (beginP && id) await >>= \case
--   --       (False, e) -> leftover e
--   --       (True, e) -> yield e >> itemLoop

--   --   itemLoop = do
--   --       fmap (fmap endP) peek >>= \case
--   --           Nothing ->
--   --           Just True -> yield e
--   --           (False, e) -> yield e

-- takeStream, takeDocument, takeNode, takeMapping, takeSequence :: (Monad m) => EventConduit m
-- takeStream   = bracketTakeMany "Expected stream end"   takeStreamStart   takeStreamEnd   takeDocument
-- takeDocument = bracketTakeMany "Expected document end" takeDocumentStart takeDocumentEnd takeNode
-- takeSequence = bracketTakeMany "Expected sequence end" takeSequenceStart takeSequenceEnd takeNode
-- takeMapping  = bracketTakeMany "Expected mapping end"  takeMappingStart  takeMappingEnd  (takeNode >> takeNode)
-- takeNode     = takeAlias <|> takeScalar <|> takeSequence <|> takeMapping

-- takeAlias, takeScalar :: (Monad m) => EventConduit m
-- takeAlias  = takeEventSatisfy $ \case (EventAlias _)        -> True; _ -> False
-- takeScalar = takeEventSatisfy $ \case (EventScalar _ _ _ _) -> True; _ -> False

-- -- | Forward an event equal to the one supplied. (and fail on any other event)
-- takeEventEq :: (Monad m)=> Event -> EventConduit m
-- takeEventEq x = takeEventSatisfy $ (x ==)

-- -- | Forward an event if it satisfies a predicate. (else fail)
-- takeEventSatisfy :: (Monad m)=> (Event -> Bool) -> EventConduit m
-- takeEventSatisfy pred = lift await >>= \case
--     Nothing -> empty
--     Just e -> if pred e then pure () else empty

-- -- XXX AAAAAAAAAAAAAAAAAAGH
-- --     This function is impossible to implement.
-- --     You can't make a ConduitParser out of a Conduit.
-- --     There goes MY ENTIRE FUCKING DAY.
-- --
-- -- | By analogy to 'takeC', constructs 'takeExactlyC'.
-- --   This turns an EventConduit into a full-fledged ParserCont.
-- exactly :: (forall m. (Monad m)=> EventConduit m) -> ParserCont
-- exactly takeStuff p = _ $ (() <$ runMaybeT takeStuff) .| (ConduitParser.runConduitParser p << sinkNull)


------------------------
-- Helpers for the pair-delimited nonterminals
bracketStream, bracketDocument :: ParserCont
bracketStream   = Parser.between streamStart streamEnd
bracketDocument = Parser.between documentStart documentEnd

-- I currently do not stand to benefit in any fashion from supporting anchors.
-- These are used in places where an anchor COULD meaningfully affect parsing,
--  but are not explicitly supported by this module.
bracketAnchorlessSequence, bracketAnchorlessMapping :: ParserCont
bracketAnchorlessSequence = Parser.between anchorlessSequenceStart sequenceEnd
bracketAnchorlessMapping = Parser.between anchorlessMappingStart mappingEnd

------------------------

-- | Supply the continuation parser with the events for the value associated
--   with a specific key in a mapping.
--
--   Failure behavior is not currently specified.
limitMappingValueImpl :: ByteString -> ParserCont
limitMappingValueImpl label cont = named "mappingValue" parser
  where
    parser = bracketAnchorlessMapping succeedEventually

    succeedEventually = succeedNow <|> succeedLater
    succeedNow = scalarStringLiteral_ label >> limitNode cont << mappingInner_
    succeedLater = node_ >> node_ >> succeedEventually

-- Feed a single element's events into a parser.
--
-- FIXME I don't even know where to begin with these.
--       I thought the EventConduit stuff above would help implement these,
--         but you can see how well that worked out.
--       (if you don't see, search this file for "exactly ::"...)
--
-- << bangs head against wall repeatedly >>
limitStream, limitDocument, limitNode, limitSequence, limitMapping :: ParserCont
limitStream   = id -- error "TODO: limitStream"
limitDocument = id -- error "TODO: limitDocument"
limitNode     = id -- error "TODO: limitNode"      -- <-- THE SINGLE MISSING PIECE :(
limitSequence = id -- error "TODO: limitSequence"  --     (I think)
limitMapping  = id -- error "TODO: limitMapping"

-- | Skip an event equal to the one supplied. (and fail on any other event)
eventEq :: Event -> Ignore
eventEq x = eventSatisfy_ $ (x ==)

-- | Skip an event if it satisfies a predicate. (else fail)
eventSatisfy_ :: (Event -> Bool) -> Ignore
eventSatisfy_ pred = eventSatisfy pred $> ()

-- | Parse an event if it satisfies a predicate. (else fail)
eventSatisfy :: (Monad m)=> (Event -> Bool) -> EventParser m Event
eventSatisfy pred = ConduitParser.peek >>= \e -> case fmap pred e of Just True -> ConduitParser.await
                                                                     _         -> Parser.unexpected "not satisfy"

-- | Parse an event into whatever your dreams are made of. (else fail)
eventParse :: (Monad m)=> (Event -> Maybe a) -> EventParser m a
eventParse func = ConduitParser.peek >>= \e -> case e >>= func of Just e  -> ConduitParser.await >> pure e
                                                                  Nothing -> Parser.unexpected "not parse"
