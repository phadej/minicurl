{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP     #-}
module MiniCurl (
    CURL,
    withLibcurl,
    withCurl,
    curlPerform,
    curlResponseCode,
) where

import Control.Exception        (bracket, bracket_)
import Data.ByteString.Internal (ByteString (..))
import Data.Coerce              (coerce)
import Data.Word                (Word8)
import Foreign.C.String         (CString, withCString)
import Control.Concurrent.MVar
import Foreign.C.Types          (CInt (..), CSize (..))
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr              (Ptr)
import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)

-- | Curl handle.
newtype CURL = CURL (MVar (Ptr Curl))

-- | (Globally) initialize @libcurl@.
--
-- Wrap your @main@ in it:
--
-- @
-- main :: IO ()
-- main = 'withLibcurl' $ do
--     ...
-- @
--
withLibcurl :: IO r -> IO r
withLibcurl = bracket_ c_minicurl_global_init c_minicurl_global_cleanup

-- | Create curl handle.
--
-- Note: you can reuse 'CURL' handle for multiple requests.
withCurl :: (CURL -> IO r) -> IO r
withCurl kont = bracket c_minicurl_init c_minicurl_cleanup $ \ptr ->
    newMVar ptr >>= coerce kont

-- | Perform request.
--
-- The resulting 'ByteString' will be exactly of the size specified by size argument.
-- If response is smaller, the rest will be zeros; if larger the response will be truncated (not read further)!
-- It's your job to verify that transport was successful, e.g. if you know the expected hash of the download.
--
-- 'curlPerform' is thread-safe (underlying handle in 'CURL' is wrapped in 'MVar').
--
curlPerform
    :: CURL     -- ^ CURL handle
    -> String   -- ^ URL
    -> Int      -- ^ Expected size of the output.
    -> IO ByteString
curlPerform (CURL curlMVar) url size = withMVar curlMVar $ \curl -> do
    fptr <- mallocPlainForeignPtrBytes size
    withForeignPtr fptr $ \ptr ->
        withCString url $ \c_url -> do
            _res <- c_minicurl_perform curl c_url ptr (fromIntegral size :: CSize)
            return ()

#if MIN_VERSION_bytestring(0,11,0)
    return $ BS fptr size
#else
    return $ PS fptr 0 size
#endif

-- | Get (last) response code.
curlResponseCode :: CURL -> IO Int
curlResponseCode (CURL curlMVar) = withMVar curlMVar $ \curl ->
    fromIntegral <$> c_minicurl_response_code curl

-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

data Curl

foreign import capi safe "hs_minicurl.h hs_minicurl_global_init" c_minicurl_global_init :: IO CInt
foreign import capi safe "hs_minicurl.h hs_minicurl_global_cleanup" c_minicurl_global_cleanup :: IO ()
foreign import capi safe "hs_minicurl.h hs_minicurl_init" c_minicurl_init :: IO (Ptr Curl)
foreign import capi safe "hs_minicurl.h hs_minicurl_cleanup" c_minicurl_cleanup :: Ptr Curl -> IO ()
foreign import capi safe "hs_minicurl.h hs_minicurl_perform" c_minicurl_perform :: Ptr Curl -> CString -> Ptr Word8 -> CSize -> IO CInt
foreign import capi safe "hs_minicurl.h hs_minicurl_response_code" c_minicurl_response_code :: Ptr Curl -> IO CInt
