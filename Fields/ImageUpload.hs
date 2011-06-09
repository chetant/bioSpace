{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Fields.ImageUpload where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Monad(liftM)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Trans.RWS(tell)
import Control.Applicative((<$>))
import Control.Exception(IOException, try, throw, ioError)
import System.FilePath(takeExtension, (</>), splitFileName)
import System.IO(openTempFile, hClose)
import Data.Text(Text, pack, unpack)
import Graphics.GD
import Text.Hamlet.NonPoly (html)
import Data.Monoid (mempty)
import Data.Maybe(fromMaybe)
import System.IO.Unsafe(unsafePerformIO)
import Debug.Trace(trace)

import Settings(staticdir)
import BioSpace

imageFieldGeneric :: (MonadIO m, Monad m2) => 
                     (Maybe a -> FormResult a)
                  -> (Maybe a -> FormResult a)
                  -> (Text -> a)
                  -> (Maybe a -> Maybe Text)
                  -> FilePath
                  -> FieldSettings Text 
                  -> Maybe a
                  -> AForm ([FieldView (GGWidget master m2 ())] -> [FieldView (GGWidget master m2 ())]) master (GGHandler sub master m) a
imageFieldGeneric noRes badRes f g savedir FieldSettings {..} mval = formToAForm $ do
  tell Multipart
  mf <- askFiles
  name <- maybe newFormIdent return fsName
  theId <- lift $ maybe (liftM pack newIdent) return fsId
  let mfi = maybe Nothing (lookup name) mf
      mimg = g mval
  res <- flip (maybe (return $ noRes mval)) mfi 
         (\fi -> do
            furl <- liftIO $ storeImage savedir fi
            return $ maybe (badRes mval) (FormSuccess . f . pack) furl
         )
  return (res, FieldView
          { fvLabel = toHtml fsLabel
          , fvTooltip = toHtml <$> fsTooltip
          , fvId = theId
          , fvInput = addHamlet [hamlet|
                                         <div>
                                             $maybe img <- mimg
                                                 <img src=/static/uploads/#{img}>
                                             <input type=file id=#{theId} name=#{name}>
                                 |]
          , fvErrors = 
              case res of
                FormFailure [e] -> Just $ toHtml e
                _ -> Nothing
          , fvRequired = True
          })

-- imageFieldReq :: (MonadIO m, Monad m2) => 
--                  FieldSettings Text 
--               -> Maybe Text
--               -> AForm ([FieldView (GGWidget master m2 ())] -> [FieldView (GGWidget master m2 ())]) master (GGHandler sub master m) Text
-- imageFieldReq = imageFieldGeneric 
--                 (const FormMissing)
--                 (maybe (FormFailure ["Not a valid image"]) FormSuccess) 
--                 id 
--                 id 
--                 (staticdir </> "uploads")


imageFieldOpt :: (MonadIO m, Monad m2) => 
                 FieldSettings Text 
              -> Maybe (Maybe Text)
              -> AForm ([FieldView (GGWidget master m2 ())] -> [FieldView (GGWidget master m2 ())]) master (GGHandler sub master m) (Maybe Text)
imageFieldOpt = imageFieldGeneric 
                (const FormMissing)
                (\img -> maybe (FormSuccess Nothing) FormSuccess (test img))
                Just 
                (maybe Nothing id) 
                (staticdir </> "uploads")
    where test a
              | trace ("Got:" ++ show a) True = a

storeImage :: FilePath -> FileInfo -> IO (Maybe FilePath)
storeImage uploadDir fi = do
  let fname = fileName fi
      imgdata = fileContent fi
      ext = drop 1 . takeExtension . unpack
      loadImg "jpeg" = loadJpegByteString . S.concat . L.toChunks
      loadImg "jpg" = loadJpegByteString . S.concat . L.toChunks
      loadImg "png" = loadPngByteString . S.concat . L.toChunks
      loadImg "gif" = loadGifByteString . S.concat . L.toChunks
      loadImg _ = throw (userError "")
      handleError :: IOException -> Maybe FilePath
      handleError _ = Nothing
  either handleError
         Just
         <$> (try $ do
                img <- loadImg (ext fname) imgdata
                (filename, hFile) <- openTempFile uploadDir (unpack fname)
                L.hPut hFile imgdata
                hClose hFile
                return (snd . splitFileName $ filename))
