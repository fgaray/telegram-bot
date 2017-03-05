module Logger (
    LoggerSet,
    LogStr,
    startLogger,
    log
    ) where


import Prelude hiding (log)
import System.Log.FastLogger
import Web.Telegram.API.Bot
import Control.Monad.IO.Class


startLogger :: IO LoggerSet
startLogger = newFileLoggerSet defaultBufSize "bot_log.txt"

log :: LoggerSet -> LogStr -> TelegramClient ()
log set str = liftIO $ pushLogStrLn set str
