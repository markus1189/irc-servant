{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower)
import           Network
import           Network.IRC.Base ( Message(Message)
                                  , Prefix(NickName)
                                  )
import           Network.IRC.Parser (decode)
import           System.Exit
import           System.IO
import           System.Process
import           System.Random (randomIO)

type IRC a = ReaderT Handle IO a

runIRC :: Handle -> IRC a -> IO a
runIRC h m = runReaderT m h

pattern PING s <- Message _ "PING" [s]
pattern JOIN c <- Message _ "JOIN" [c]
pattern PRIVMSG c s <- Message _ "PRIVMSG" [c,s]

pattern COMMAND c cmd <- PRIVMSG c (B.unpack -> '>':' ':cmd)
pattern SAY_HELLO c <- COMMAND c (map toLower -> "say hello")
pattern SAY_TIME c <- COMMAND c (map toLower -> "what time is it?")
pattern LEAVE c <- COMMAND c (map toLower -> "get lost")

pattern FROM_MASTER <- Message (Just (NickName (isMaster -> True) _ _)) _ _

ircPutStrLn :: B.ByteString -> IRC ()
ircPutStrLn s = do
  h <- ask
  liftIO $ B.hPutStrLn h s

ircGetLine :: IRC B.ByteString
ircGetLine = do
  h <- ask
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

  rndNumber <- randomIO :: IO Int

  runIRC h $ do
    nick $ B.append "IRCBot" (B.pack . show $ rndNumber)
    user $ B.append "IRCBot" (B.pack . show $ rndNumber)

    joinChan "##os2-lab"

    forever loop

sendMsg :: B.ByteString -> B.ByteString -> IRC ()
sendMsg c s = ircPutStrLn . B.concat $ ["PRIVMSG ", c, " :",s]

isMaster :: B.ByteString -> Bool
isMaster s = s == "predator117" || s == "predator217"

loop :: IRC ()
loop = do
  line <- ircGetLine
  flip (maybe $ return ()) (decode line) $ \msg ->
    case msg of
      FROM_MASTER -> handleMasterMsg msg line
      _ -> handleMsg msg line

handleMasterMsg :: Message -> B.ByteString -> IRC ()
handleMasterMsg msg line = case msg of
  SAY_HELLO c -> sendMsg c "Hello everybody."
  SAY_TIME c -> (liftIO $ readProcess "date" [] []) >>= (sendMsg c . B.pack)
  LEAVE _ -> do
    ircPutStrLn "QUIT :It was a pleasure."
    liftIO exitSuccess
  _ -> liftIO $ B.putStr "Don't know what about: " >> B.putStrLn line

handleMsg :: Message -> B.ByteString -> IRC ()
handleMsg msg line = case msg of
  PING s -> pong s
  JOIN c -> sendMsg c "Your faithful servant awaits commands."
  COMMAND c _ -> sendMsg c "You are not my master."
  _ -> liftIO $ B.putStr "Don't know what to do with: " >> B.putStrLn line

pong :: B.ByteString -> IRC ()
pong = ircPutStrLn . B.append "PONG :"
