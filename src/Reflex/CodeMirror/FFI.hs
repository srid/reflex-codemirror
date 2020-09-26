{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.CodeMirror.FFI where

import "lens" Control.Lens hiding (element, (#))
import "aeson" Data.Aeson (toJSON)
import "text" Data.Text (Text)
import GHCJS.DOM.Element (IsElement, toElement, unElement)
import "jsaddle" Language.Javascript.JSaddle
import Reflex.CodeMirror.Types

newtype CodeMirrorRef = CodeMirrorRef
  { unCodeMirrorRef :: JSVal
  }

--
fromTextArea ::
  (IsElement element) =>
  -- | Element to use
  element ->
  -- | CodeMirror configuration
  Configuration ->
  JSM CodeMirrorRef
fromTextArea
  element_
  config = do
    let js_element = unElement . toElement $ element_
    let js_config = toJSON $ config
    ref <- nextAnimationFrame $ \_ -> (jsg "CodeMirror") # "fromTextArea" $ (js_element, js_config)
    return $ CodeMirrorRef ref

registerOnChange ::
  -- | ref
  CodeMirrorRef ->
  -- | Callback with Text
  (Text -> JSM ()) ->
  JSM ()
registerOnChange codeMirrorRef callback = do
  codemirror <- valToObject . unCodeMirrorRef $ codeMirrorRef
  _ <-
    codemirror
      ^. js2
        "on"
        "change"
        ( fun $ \_ _ _ -> do
            x <- codemirror ^. js0 "getValue"
            t <- valToText x
            -- _ <- valToText =<< codemirror ^. js0 "getValue"
            callback t
            return ()
        )
  return ()

setValue ::
  -- | ref
  CodeMirrorRef ->
  -- | value
  Text ->
  JSM ()
setValue ref text = do
  codemirror <- valToObject . unCodeMirrorRef $ ref
  --js_text <- toJSON $ text
  _ <- codemirror ^. js1 "setValue" text
  return ()

scrollIntoView ::
  -- | ref
  CodeMirrorRef ->
  -- | Line & Char
  LineChar ->
  -- | delay ms
  Int ->
  JSM ()
scrollIntoView ref lc delay = do
  codemirror <- valToObject . unCodeMirrorRef $ ref
  let js_lc = toJSON $ lc
  _ <- codemirror ^. js2 "scrollIntoView" js_lc delay
  return ()
