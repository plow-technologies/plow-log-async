{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plow.Logging.Async (withAsyncHandleTracer) where

import qualified Control.Monad.IO.Class
import Data.Conduit ((.|))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.TMChan as Conduit.TMChan
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time (getCurrentTime)
import Plow.Logging (IOTracer (..), Tracer (..), traceWith)
import System.IO (Handle, hFlush)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (link, withAsync)
import qualified UnliftIO.STM as STM

-- | Returns (in CPS) a 'IOTracer' that pushes messages to a thread-safe queue.
-- This 'IOTracer' won't block unless the queue is full (size is configurable with
-- queueSize)
--
-- An async thread that continuously consumes traces in queue by printing them to a
-- 'Handle' will be launched. Any exceptions thrown inside (or to) the thread will be
-- rethrown in the caller of this function
--
-- Example use
--
-- main =
--   withAsyncHandleTracer stdout 100 $ \tracer' -> do
--     -- We use contramap to convert the tracer to a tracer that accepts
--     -- domain-specic trace types and displays them as Text
--     let tracer = contramap displaySomeTrace tracer'
--     traceWith tracer (SomeTrace a b c)
--     ...
withAsyncHandleTracer :: MonadUnliftIO m => Handle -> Int -> (IOTracer Text -> m a) -> m a
withAsyncHandleTracer handle queueSize f = do
  chan <- STM.atomically $ Conduit.TMChan.newTBMChan queueSize
  withAsync (logConsumer chan) $ \logConsumerThread -> do
    let tracer = asyncTracer chan
    link logConsumerThread
    res <- f tracer
    traceWith tracer "exit" >> waitUntilEmpty chan >> return res
  where
    logConsumer chan =
      Conduit.runConduit $
        ( Conduit.TMChan.sourceTBMChan chan
            .| Conduit.awaitForever
              ( \(time, msg) ->
                  Control.Monad.IO.Class.liftIO $ do
                    T.hPutStrLn handle $ fromString (show time <> ": ") <> msg
                    hFlush handle
              )
        )

    asyncTracer chan = IOTracer $
      Tracer $ \msg -> do
        time <- Control.Monad.IO.Class.liftIO $ getCurrentTime
        Conduit.runConduit $
          Conduit.yield (time, msg)
            .| Conduit.TMChan.sinkTBMChan chan

    waitUntilEmpty chan =
      STM.atomically $
        Conduit.TMChan.isEmptyTBMChan chan >>= \case
          True -> return ()
          False -> STM.retrySTM
