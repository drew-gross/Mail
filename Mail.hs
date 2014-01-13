{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mail (
	sendmail,
	addHeader,
	contentType,
	subject,
	body,
	to,
	toMany,
	toMore,
	from,
	emptymail
) where

import System.Directory (doesFileExist, getPermissions, executable)
import System.IO (hPutStr)
import System.Cmd.Utils (pOpen, PipeMode(WriteToPipe))
import Control.Lens hiding (to, from)
import Control.Monad.State (get, put, State, runState, modify)
import Control.Exception (try, IOException)
import Data.Map (Map, insert, empty, foldWithKey)

data Mail = Mail { _headers :: Map String String
				 , _message :: String
				 , _fromAddress :: String
				 , _toAddresses :: [String]
		    	 } deriving (Show)

makeLenses ''Mail

emptymail = Mail {_headers = empty, 
				  _message = "",
				  _fromAddress = "",
				  _toAddresses = []}

sendmails :: [String]
sendmails = ["/usr/sbin/sendmail",
             "/usr/local/sbin/sendmail",
             "/usr/local/bin/sendmail",
             "/usr/bin/sendmail",
             "/etc/sendmail",
             "/usr/etc/sendmail"]

findsendmail :: IO String
findsendmail = worker sendmails
	where
    	worker [] = return "sendmail"
        worker (this:next) = do
            e <- doesFileExist this
            if e then do
               	p <- getPermissions this
               	if executable p then
                	return this
                else
                	worker next
            else 
            	worker next

addHeader :: String -> String -> State Mail ()
addHeader header value = modify $ headers . at header ?~ value

contentType = addHeader "Content-Type"
subject = addHeader "Subject"

body :: String -> State Mail ()
body text = message .= text

from :: String -> State Mail ()
from address = fromAddress .= address

to :: String -> State Mail ()
to address = toAddresses .= [address]

toMany :: [String] -> State Mail ()
toMany addresses = toAddresses .= addresses

toMore :: [String] -> State Mail ()
toMore addresses = toAddresses %= (++ addresses)

sendmail :: State Mail () -> IO ()
sendmail mailBuilder = sendmail_worker (fromArg : toArgs) (msgHeaders ++ body)
	where
		mail = snd $ runState mailBuilder emptymail
		fromArg = if null $ mail ^. fromAddress then
					  "" 
				  else 
				  	  "-f" ++ mail ^. fromAddress
		toArgs = mail ^. toAddresses
		msgHeaders = foldWithKey folder "" $ (mail ^. headers)
			where
				folder key value acc = key ++ ": " ++ value ++ "\n" ++ acc
		body = mail ^. message

sendmail_worker :: [String] -> String -> IO ()
sendmail_worker args msg = do
	result <- try (pOpen WriteToPipe "sendmail" args func)
	case result of
		Right x -> return x
		Left (_ :: IOException) -> do
			sender <- findsendmail
			result <- pOpen WriteToPipe sender args func
			return $! result
	where
		func handle = hPutStr handle msg