{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
import           Data.Char (toLower)
import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import           Network
import           Network.IRC.Parser (decode)
import           Network.IRC.Base ( Message(Message, msg_prefix)
                                  , Prefix(NickName)
                                  )
import           System.IO
import           System.Exit
import           System.Random (randomIO)
import           System.Process

pattern PING s <- Message _ "PING" [s]
pattern JOIN c <- Message _ "JOIN" [c]
pattern PRIVMSG c s <- Message _ "PRIVMSG" [c,s]

pattern COMMAND c cmd <- PRIVMSG c (B.unpack -> '>':' ':cmd)
pattern SAY_HELLO c <- COMMAND c (map toLower -> "say hello")
pattern SAY_TIME c <- COMMAND c (map toLower -> "what time is it?")
pattern LEAVE c <- COMMAND c (map toLower -> "get lost")

nick :: Handle -> String -> IO ()
nick h name = hPutStrLn h ("NICK " ++ name)

user :: Handle -> String -> IO ()
user h name = hPutStrLn h ("USER " ++ name ++ " 0 * :" ++ name)

joinChan :: Handle -> String -> IO ()
joinChan h c = hPutStrLn h ("JOIN " ++ c)

main :: IO ()
main = do
  h <- connectTo "irc.freenode.org" (PortNumber 6667)
  hSetBuffering   h NoBuffering
  hSetNewlineMode h (NewlineMode CRLF CRLF)

  rndNumber <- randomIO :: IO Int

  nick h $ "IRCBot" ++ show rndNumber
  user h $ "IRCBot" ++ show rndNumber

  joinChan h "##os2-lab"

  forever (loop h)

sendMsg :: Handle -> B.ByteString -> String -> IO ()
sendMsg h c s = B.hPutStrLn h (B.pack $ "PRIVMSG " ++ B.unpack c ++ " :" ++ s)

isMaster :: B.ByteString -> Bool
isMaster s = s == "predator117" || s == "predator217"

loop :: Handle -> IO ()
loop h = do
  line <- B.hGetLine h
  flip (maybe $ return ()) (decode line) $ \msg ->
    case msg_prefix msg of
      Just (NickName (isMaster -> True) _ _) ->
        case msg of
          SAY_HELLO c -> sendMsg h c "Hello everybody."
          SAY_TIME c -> readProcess "date" [] [] >>= sendMsg h c
          LEAVE _ -> do
            B.hPutStrLn h . B.pack $ "QUIT :It was a pleasure."
            exitSuccess
          _ -> B.putStr "Don't know what about: " >> B.putStrLn line
      _ -> case msg of
        PING s -> B.hPutStrLn h . B.pack $ "PONG :" ++ B.unpack s
        JOIN c -> sendMsg h c "Your faithful servant awaits commands."
        COMMAND c _ -> sendMsg h c "You are not my master."
        _ -> B.putStr "Don't know what about: " >> B.putStrLn line
