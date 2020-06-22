{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import                     Prelude hiding (head)
import "lens"              Control.Lens
import "data-default"      Data.Default (def)
import "aeson"             Data.Aeson (toJSON, Value(..))
import "text"              Data.Text (Text, pack)
import "reflex-dom"        Reflex.Dom
import "reflex-utils"      Reflex.Utils
import "reflex-codemirror" Reflex.CodeMirror

--
main :: IO ()
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head
            whenLoaded [headD] blank body
            return ()

--
head :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.css"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/theme/zenburn.css"
                     ]
    whenLoaded s1Ds blank $ do
        sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/mode/haskell/haskell.min.js"
                 ]
        return ()

--
body :: MonadWidget t m => m ()
body = do
    clickE <- button "goto line 3"
    let lineCharE = (Just $ LineChar 3 1) <$ clickE
    click2E <- button "change text"
    let textE = ("from event" <$ click2E)
    textE <- codemirror config textE lineCharE
    textD <- holdDyn "" textE
    display textD
    where
        config :: Configuration
        config
            = def
            & configuration_value ?~ pack "hello\nworld"
            & configuration_theme ?~ pack "zenburn"
            & configuration_mode  ?~ (String $ pack "haskell")

