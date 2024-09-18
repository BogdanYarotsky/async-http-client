import Network.Socket
import System.IO

data Uri = Uri
  { host :: String,
    path :: String
  }

data VersionPolicy = OrLower | OrHigher | Exact

-- Define the HttpRequest record
data HttpRequest = HttpRequest
  { content :: String,
    headers :: [(String, String)],
    method :: String,
    options :: [(String, String)],
    uri :: Uri,
    version :: String,
    versionPolicy :: VersionPolicy
  }

-- Define the HTTP request string
httpRequest :: Uri -> String
httpRequest uri' =
  unlines
    ["GET " ++ path uri' ++ " HTTP/1.1", "Host: " ++ host uri', "Connection: close\n\n"]

-- Connect to the host and port
connectToHost :: String -> String -> IO Handle
connectToHost host' port = do
  addrInfos <- getAddrInfo Nothing (Just host') (Just port)
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  return handle

-- Send an HTTP request and get the response
simpleHttpGet :: Uri -> IO String
simpleHttpGet uri' = do
  handle <- connectToHost (host uri') "80"
  hPutStr handle (httpRequest uri')
  hGetContents handle

-- Main function to demonstrate fetching a page
main :: IO ()
main = do
  let host' = "example.com"
  let path' = "/"
  response <- simpleHttpGet (Uri host' path')
  putStrLn response