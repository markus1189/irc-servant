{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, asks, runReaderT)
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower)
import           Data.Functor ((<$>))
import           Data.Traversable (traverse)
import           Network
import           Network.IRC.Base ( Message(Message)
                                  , Prefix(NickName)
                                  )
import           Network.IRC.Parser (decode)
import           System.Exit
import           System.IO
import           System.Process
import           System.Random (randomIO)

type IRC a = ReaderT IRCConfig IO a

runIRC :: IRCConfig -> IRC a -> IO a
runIRC cfg m = runReaderT m cfg

data IRCConfig = IRCConfig { _handle :: Handle
                           , _name :: B.ByteString
                           , _channels :: [B.ByteString]
                           }

defaultConfig :: Handle -> IRCConfig
defaultConfig h = IRCConfig h "IrcServant" ["##markus-irc-bot","##os2-lab"]

pattern PING s <- Message _ "PING" [s]
pattern JOIN c <- Message _ "JOIN" [c]
pattern PRIVMSG c s <- Message _ "PRIVMSG" [c,s]

pattern COMMAND c cmd <- PRIVMSG c (B.unpack -> '>':' ':cmd)
pattern SAY_HELLO c <- COMMAND c (map toLower -> "say hello")
pattern SAY_TIME c <- COMMAND c (map toLower -> "what time is it?")
pattern LEAVE c <- COMMAND c (map toLower -> "get lost")

messageFromMaster :: Message -> Bool
messageFromMaster (Message (Just (NickName (isMaster -> True) _ _)) _ _) = True
messageFromMaster _ = False

ircPutStrLn :: B.ByteString -> IRC ()
ircPutStrLn s = do
  h <- asks _handle
  liftIO $ B.hPutStrLn h s

ircGetLine :: IRC B.ByteString
ircGetLine = do
  h <- asks _handle
  liftIO $ B.hGetLine h

nick :: B.ByteString -> IRC ()
nick = ircPutStrLn . B.append "NICK "

user :: B.ByteString -> IRC ()
user name = ircPutStrLn . B.concat $ ["USER ", name, " 0 * :", name]

joinChan :: B.ByteString -> IRC ()
joinChan = ircPutStrLn . B.append "JOIN "

main :: IO ()
main = do
  h <- connectTo "irc.freenode.org" (PortNumber 6667)
  hSetBuffering   h NoBuffering
  hSetNewlineMode h (NewlineMode CRLF CRLF)

  rndNumber <- (B.pack . show) <$> (randomIO :: IO Int)

  runIRC (defaultConfig h) $ do
    n <- asks _name
    let s = B.append n rndNumber

    nick s >> user s

    cs <- asks _channels

    traverse joinChan cs

    forever loop

sendMsg :: B.ByteString -> B.ByteString -> IRC ()
sendMsg c s = ircPutStrLn . B.concat $ ["PRIVMSG ", c, " :",s]

isMaster :: B.ByteString -> Bool
isMaster s = s == "predator117" || s == "predator217"

loop :: IRC ()
loop = do
  line <- ircGetLine
  onJust (decode line) $ \msg ->
    (if messageFromMaster msg then handleMasterMsg else handleMsg line) msg
  where onJust Nothing _ = return ()
        onJust (Just msg) f = (f msg)

handleMasterMsg :: Message -> IRC ()
handleMasterMsg msg = case msg of
  SAY_HELLO c -> sendMsg c "Hello everybody."
  SAY_TIME c -> (liftIO $ readProcess "date" [] []) >>= (sendMsg c . B.pack)
  LEAVE _ -> quit "It was a pleasure." >> liftIO exitSuccess
  COMMAND c _ -> sendMsg c "Sorry I don't know that command."

handleMsg :: B.ByteString -> Message -> IRC ()
handleMsg line msg = case msg of
  PING s -> pong s
  JOIN c -> sendMsg c "Your faithful servant awaits commands."
  COMMAND c _ -> sendMsg c "You are not my master."
  _ -> liftIO $ B.putStrLn line

pong :: B.ByteString -> IRC ()
pong = ircPutStrLn . B.append "PONG :"

quit :: B.ByteString -> IRC ()
quit = ircPutStrLn . B.append "QUIT: "
