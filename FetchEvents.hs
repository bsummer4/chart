{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UnicodeSyntax            #-}

module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Monoid
import           Control.Monad.IO.Class
import qualified Data.Aeson              as JSON
import qualified Data.Aeson.TH           as JSON
import qualified Data.ByteString.Lazy    as LBS
import           Data.Default
import qualified Data.List               as List
import           Data.String.Conversions (cs)
import           Data.Text
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time.Clock         (DiffTime, secondsToDiffTime)
import           Data.Time.Clock         (UTCTime)
import           Debug.Trace
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.JQuery

type LazyByteString = LBS.ByteString


-- Foreign Imports -------------------------------------------------------------

foreign import javascript interruptible
  "gapi_authorize($1,$2,$3,$c);"
  js_gapi_authorize ∷ JSString → JSArray JSString → JSBool → IO JSBool

foreign import javascript unsafe
  "console.log($1);"
  js_log ∷ JSRef a → IO ()

foreign import javascript interruptible
  "gapi.client.load('calendar', 'v3', $c);"
  js_gapi_client_load ∷ IO (JSRef a)

foreign import javascript interruptible
  "august_events($c);"
  js_august_events ∷ IO (JSArray a)

foreign import javascript unsafe
  "JSON.stringify($1)"
  js_json_stringify ∷ JSRef o → IO JSString


-- Types -----------------------------------------------------------------------

data Credentials = Credentials { clientID  ∷ !Text
                               , scopes    ∷ ![Text]
                               }

data GCalEvent = GCalEvent { start       ∷ UTCTime
                           , end         ∷ UTCTime
                           , colorId     ∷ Int
                           , summary     ∷ Text
                           , description ∷ Text
                           } deriving (Show,Eq,Ord)

$(JSON.deriveJSON JSON.defaultOptions ''GCalEvent)


-- Basic Values ----------------------------------------------------------------

creds ∷ Credentials
creds =
  Credentials
    "513026491729-icj865qne4cbe8d65m70c7kvq084ahus.apps.googleusercontent.com"
    ["https://www.googleapis.com/auth/calendar.readonly"]


-- Application -----------------------------------------------------------------

data Auth = Auth

data AuthMethod = Cache | PopUp deriving Show

runMaybe ∷ Monad m ⇒ (a → m ()) → Maybe a → m ()
runMaybe _ Nothing  = return ()
runMaybe a (Just x) = a x

waitForSuccess ∷ ((Maybe a → IO ()) → IO ()) → IO a
waitForSuccess register = do resultV ← newEmptyMVar
                             register $ runMaybe $ putMVar resultV
                             takeMVar resultV

authenticate ∷ Credentials → (IO () → IO ()) → IO Auth
authenticate creds setupButton = checkCache >>= maybe popUpAuth return
  where checkCache = authorize creds Cache
        popUpAuth = waitForSuccess $ setupButton . (authorize creds PopUp >>=)

main = do
  traceM "Haskell code is now loaded"

  authButton ← select "#authorize"
  Auth ← authenticate creds $ \action →
    void $ click (const action) def authButton

  traceM "Main thread is now authenticated"

  traceM "Loading Google's Calendar library."

  void $ js_gapi_client_load

  traceM "Getting a list of events."

  events ← js_august_events >>= marshallEvents

  traceM "Printing all events."

  print events

  traceM "Done!"

fromJSText ∷ JSString → Text
fromJSText = fromJSString

marshallEvent ∷ JSRef event → IO (Maybe GCalEvent)
marshallEvent = fmap (JSON.decode . cs . fromJSText) . js_json_stringify

marshallEvents ∷ JSRef events → IO (Maybe [GCalEvent])
marshallEvents = fmap (JSON.decode . traceShowId . cs . fromJSText) . js_json_stringify

toJSArray ∷ [JSRef a] → IO (JSArray (JSRef a))
toJSArray = toArray <=< mapM toJSRef

gapi_authorize ∷ Credentials → Bool → IO Bool
gapi_authorize creds immediate = do
  fmap fromJSBool $ join $ js_gapi_authorize
    <$> (pure      $ toJSString $ clientID creds)
    <*> (toJSArray $ toJSString <$> scopes creds)
    <*> (pure      $ toJSBool immediate)

authorize ∷ Credentials → AuthMethod → IO (Maybe Auth)
authorize c method = do
  traceM $ "Attempting to authorize using method: " <> show method
  let immediate = case method of Cache→True; PopUp→False
  success ← gapi_authorize creds immediate
  case success of
    True → do traceM $ "Sucessfully authenticated using method: " <> show method
              return (Just Auth)
    False → do traceM $ "Failed to authenticate using method: " <> show method
               return Nothing
