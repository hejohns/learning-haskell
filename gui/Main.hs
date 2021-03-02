{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified GI.Gtk as Gtk
import Data.GI.Base

activate :: Gtk.Application -> IO ()
activate app = do
    win <- new Gtk.ApplicationWindow [#application := app, #title := "title"]
    #show win

main :: IO ()
main = do
    app <- new Gtk.Application [#applicationId := "edu.umich.hejohns.xournal"]
    on app #activate (activate app)
    #run app Nothing >> return ()
