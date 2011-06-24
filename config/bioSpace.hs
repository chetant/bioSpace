{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withBioSpace)
-- import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.FastCGI (run)

main :: IO ()
-- main = withBioSpace $ run 3000
main = withBioSpace run
#else
import Controller (withBioSpace)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withBioSpace $ run port . debug
#endif
