{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Reflex.CodeMirror
  ( module Reflex.CodeMirror.Types,
    module Reflex.CodeMirror.FFI,
    codemirror,
  )
where

import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "base" Data.IORef (IORef, newIORef, readIORef, writeIORef)
import "text" Data.Text (Text)
-- (JSVal) --  GHCJS.Types (JSVal)

import GHCJS.DOM.Element hiding (scrollIntoView) -- (IsElement)
import "jsaddle" Language.Javascript.JSaddle
import Reflex.CodeMirror.FFI
import Reflex.CodeMirror.Types hiding (configuration)
import "reflex-dom" Reflex.Dom hiding (setValue)

codemirror ::
  forall t m.
  ( DomBuilder t m,
    MonadIO m,
    PostBuild t m,
    TriggerEvent t m,
    MonadJSM m,
    PerformEvent t m,
    MonadIO (Performable m),
    IsElement (RawElement (DomBuilderSpace m))
  ) =>
  Configuration ->
  Event t Text ->
  Event t (Maybe LineChar) ->
  (CodeMirrorRef -> JSM ()) ->
  m (Event t Text)
codemirror configuration textE scrollToE initFunc = do
  -- HTML element
  (element_, _) <- el' "textarea" $ text $ maybe "" id (_configuration_value configuration)

  -- local state
  (ref :: IORef (Maybe CodeMirrorRef)) <- liftIO $ newIORef Nothing

  -- input event
  (postBuildTaggedE :: Event t ()) <- getPostBuild
  let inputE =
        leftmost
          [ (Nothing, Nothing) <$ postBuildTaggedE,
            (\x -> (Nothing, x)) <$> scrollToE,
            (\x -> (Just x, Nothing)) <$> textE
          ]

  -- output event + trigger
  (outE :: Event t Text, triggerOut) <- newTriggerEvent

  -- handle input event
  ctxRef <- askJSM
  performEvent_ $
    ffor inputE $ \(mText, mScrollTo) ->
      flip runJSM ctxRef $
        handle
          (_element_raw element_)
          ref
          triggerOut
          configuration
          mText
          mScrollTo
  return outE
  where
    handle ::
      (IsElement el) =>
      -- | Element
      el ->
      -- | Local state
      IORef (Maybe CodeMirrorRef) ->
      -- | Trigger for output event
      (Text -> IO ()) ->
      -- | Chart data
      Configuration ->
      -- | Text
      (Maybe Text) ->
      -- | Scroll to
      (Maybe LineChar) ->
      JSM ()
    handle element_ ref trigger configuration_ mText mScrollTo = do
      currentRef_ <- liftIO $ readIORef ref
      case currentRef_ of
        Nothing -> onFirstTime element_ ref trigger configuration_ mText mScrollTo
        Just ref_ -> onNextTime ref_ configuration_ mText mScrollTo

    onFirstTime ::
      (IsElement el) =>
      -- | Element
      el ->
      -- | Local state
      IORef (Maybe CodeMirrorRef) ->
      -- | Trigger for output event
      (Text -> IO ()) ->
      -- | Chart data
      Configuration ->
      -- | Text
      (Maybe Text) ->
      -- | Scroll To
      (Maybe LineChar) ->
      JSM ()
    onFirstTime element_ ref trigger configuration_ mText mScrollTo = do
      ref_ <- fromTextArea element_ configuration_
      liftIO $ writeIORef ref (Just ref_)
      registerOnChange
        ref_
        (onChangeCallback trigger)
      initFunc ref_
      case mText of
        Nothing -> return ()
        Just text_ -> setValue ref_ text_
      case mScrollTo of
        Nothing -> return ()
        Just scrollTo_ -> scrollIntoView ref_ scrollTo_ 200

      return ()

    onNextTime ::
      -- | Current value of local state
      CodeMirrorRef ->
      -- | Chart data
      Configuration ->
      -- | Text
      (Maybe Text) ->
      -- | Scroll To
      (Maybe LineChar) ->
      JSM ()
    onNextTime ref_ _ mText mScrollTo = do
      case mText of
        Nothing -> return ()
        Just text_ -> setValue ref_ text_
      case mScrollTo of
        Nothing -> return ()
        Just scrollTo_ -> scrollIntoView ref_ scrollTo_ 200

    onChangeCallback ::
      (Text -> IO ()) ->
      Text ->
      JSM ()
    onChangeCallback trigger t = do
      liftIO $ trigger t
      return ()
