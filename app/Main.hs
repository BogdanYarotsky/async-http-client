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
httpRequest :: String -> String -> String
httpRequest host path =
  unlines
    ["GET " ++ path ++ " HTTP/1.1", "Host: " ++ host, "Connection: close\n\n"]

-- Connect to the host and port
connectToHost :: String -> String -> IO Handle
connectToHost host port = do
  addrInfos <- getAddrInfo Nothing (Just host) (Just port)
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  return handle

-- Send an HTTP request and get the response
simpleHttpGet :: String -> String -> IO String
simpleHttpGet host path = do
  handle <- connectToHost host "80"
  hPutStr handle (httpRequest host path)
  hGetContents handle

-- Main function to demonstrate fetching a page
main :: IO ()
main = do
  let host = "example.com"
  let path = "/"
  response <- simpleHttpGet host path
  putStrLn response