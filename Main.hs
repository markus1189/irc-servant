{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns, TemplateHaskell #-}

import           Control.Applicative (pure)
import           Control.Lens (use)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad (forever, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (StateT, evalStateT)
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower)
import           Data.Foldable (for_, traverse_)
import           Data.Functor ((<$>))
import qualified Data.Set as Set
import           Network
import           Network.IRC.Base ( Message(Message)
                                  , Prefix(NickName)
                                  , showMessage
                                  )
import           Network.IRC.Parser (decode)
import           System.Exit
import           System.IO
import           System.Posix.Env.ByteString (getArgs)
import           System.Process
import           System.Random (randomIO)

-- makeLensesFor [ ("msg_prefix", "msgPrefix")
--               , ("msg_params", "msgParams")
--               , ("msg_command", "msgCommand")] ''Message

-- srcChannel :: Traversal' Message B.ByteString
-- srcChannel = msgParams . _head

type IRC a = StateT IRCConfig IO a

runIRC :: IRCConfig -> IRC a -> IO a
runIRC cfg m = evalStateT m cfg

data IRCConfig = IRCConfig { _cfgHandle :: Handle
                           , _cfgName :: B.ByteString
                           , _cfgChannels :: [B.ByteString]
                           , _cfgMasters :: Set.Set B.ByteString
                           , _cfgJoinedChannels :: Set.Set B.ByteString
                           }
makeLenses ''IRCConfig

defaultConfig :: Handle -> IRCConfig
defaultConfig h = IRCConfig h "IrcServant" [] Set.empty Set.empty

pattern PING s <- Message _ "PING" [s]
pattern JOIN c <- Message _ "JOIN" [c]
pattern PART c <- Message _ "PART" [c]
pattern PRIVMSG c s <- Message _ "PRIVMSG" [c,s]

lowerCase :: String -> String
lowerCase = map toLower

lowerWords :: String -> [String]
lowerWords = words . lowerCase

pattern COMMAND c cmd <- PRIVMSG c (B.unpack -> '>':' ':cmd)
pattern SAY_HELLO c <- COMMAND c (lowerCase -> "say hello")
pattern SAY_TIME c <- COMMAND c (lowerCase -> "what time is it?")
pattern LEAVE c <- COMMAND c (lowerCase -> "get lost")
pattern CONFIRMATION c <- COMMAND c (lowerCase -> "right?")
pattern LIST_MASTERS c <- COMMAND c (lowerCase -> "masters")
pattern LIST_CHANNELS c <- COMMAND c (lowerCase -> "channels")
pattern ADD_MASTER c master <- COMMAND c (lowerWords -> ["add-master",master])
pattern REMOVE_MASTER c master <- COMMAND c (lowerWords -> ["remove-master",master])
pattern JOIN_CHAN srcChan toJoinChan <- COMMAND srcChan (lowerWords -> ["join-channel",toJoinChan])
pattern PART_CHAN c <- COMMAND c (lowerCase -> "part-channel")

messageFromMaster :: Message -> IRC Bool
messageFromMaster (Message (Just (NickName n _ _)) _ _) = Set.member n <$> use cfgMasters
messageFromMaster _ = pure False

ircPutStrLn :: B.ByteString -> IRC ()
ircPutStrLn s = do
  h <- use cfgHandle
  liftIO $ B.hPutStrLn h s

ircGetLine :: IRC B.ByteString
ircGetLine = do
  h <- use cfgHandle
  liftIO $ B.hGetLine h

nick :: B.ByteString -> IRC ()
nick = ircPutStrLn . B.append "NICK "

user :: B.ByteString -> IRC ()
user name = ircPutStrLn . B.concat $ ["USER ", name, " 0 * :", name]

joinChan :: B.ByteString -> IRC ()
joinChan = ircPutStrLn . B.append "JOIN "

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Usage: irc-bot <master> <channel>..."
    else do
      h <- connectTo "irc.freenode.org" (PortNumber 6667)
      hSetBuffering h NoBuffering
      hSetNewlineMode h (NewlineMode CRLF CRLF)

      rndNumber <- (B.pack . show) <$> (randomIO :: IO Int)

      runIRC (defaultConfig h) $ do

        cfgChannels %= (++ tail args)
        cfgMasters %= Set.insert (head args)

        use cfgMasters >>= liftIO . B.putStrLn . B.intercalate ", " . Set.toList

        n <- use cfgName
        let s = B.append n rndNumber

        nick s >> user s

        chans <- use cfgChannels
        traverse_ joinChan chans

        forever loop

sendMsg :: B.ByteString -> B.ByteString -> IRC ()
sendMsg c s = ircPutStrLn . B.concat $ ["PRIVMSG ", c, " :",s]

partChan :: B.ByteString -> IRC ()
partChan chan = ircPutStrLn . B.concat $ ["PART ",chan]

loop :: IRC ()
loop = do
  line <- ircGetLine
  for_ (decode line) $ \msg -> do
    isMaster <- messageFromMaster msg
    (if isMaster then handleMasterMsg else handleMsg) msg

handleMasterMsg :: Message -> IRC ()
handleMasterMsg msg = case msg of
  LIST_MASTERS c -> do
    ms <- use cfgMasters
    sendMsg c . B.intercalate ", " . Set.toList $ ms
  LIST_CHANNELS c -> do
    use cfgJoinedChannels >>= sendMsg c . B.intercalate ", " . Set.toList
  ADD_MASTER c mas -> do
    cfgMasters %= (Set.insert . B.pack) mas
    sendMsg c $ B.append "Hello, "( B.pack mas)
  REMOVE_MASTER c mas -> do
    wasMaster <- Set.member (B.pack mas) <$> use cfgMasters
    cfgMasters %= (Set.delete . B.pack) mas
    when wasMaster $ sendMsg c "Removed."
  JOIN_CHAN src tgt -> do
    joinChan . B.pack $ tgt
    sendMsg src $ B.append "Joining " (B.pack tgt)
  PART_CHAN c -> partChan c >> sendMsg c "Goodbye."
  SAY_HELLO c -> sendMsg c "Hello everybody."
  SAY_TIME c -> (liftIO $ readProcess "date" [] []) >>= (sendMsg c . B.pack)
  LEAVE c -> do
    sendMsg c "An honor to serve."
    quit "It was a pleasure."
    liftIO exitSuccess
  CONFIRMATION c -> sendMsg c "Of course master."
  COMMAND c _ -> sendMsg c "Sorry I don't know that command."
  _ -> return ()

handleMsg :: Message -> IRC ()
handleMsg msg = case msg of
  PING s -> pong s
  JOIN c -> do
    cfgJoinedChannels %= Set.insert c
    sendMsg c "Your faithful servant awaits commands."
  PART c -> cfgJoinedChannels %= Set.delete c
  COMMAND c _ -> sendMsg c "You are not my master."
  _ -> liftIO . B.putStrLn . showMessage $ msg

pong :: B.ByteString -> IRC ()
pong = ircPutStrLn . B.append "PONG :"

quit :: B.ByteString -> IRC ()
quit = ircPutStrLn . B.append "QUIT: "
