{-GPLV3.0 or later copyright Timothy Hobbs <timothyhobbs@seznam.cz>

Copyright 2012.

This program is free software:
you can redistribute it and/or modify it
under the terms of the GNU General Public License
as published by the Free Software Foundation,
either version 3 of the License,
or
(at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY;
without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy
of the GNU General Public License along with this program.
If not,
see <http://www.gnu.org/licenses/>.
-}
module Graphics.UI.Gtk.Custom.ToggleButtonList where

{-
Simple horizontal and vertical toggle button lists.
See source for usage example.
-}

import Graphics.UI.Gtk as GTK

{-main :: IO ()
main = do
   GTK.initGUI       -- is start
   window <- GTK.windowNew
   (toggleButtonList,updater) <- toggleButtonListNew (\item->putStrLn item) Vertical (\button->GTK.buttonSetRelief button GTK.ReliefNone) ["Hi","Bye","Bar","Foo","LadyDa"]
   GTK.containerAdd window toggleButtonList
   GTK.onDestroy window GTK.mainQuit
   updater ["Bob","Fred"]
   GTK.widgetShowAll window
   GTK.mainGUI
   return ()-}

type ToggleButtonListUpdater = [String]->IO ()

data Orientation = Horizontal | Vertical

{-toggleButtonListNew ::
 (String->IO()) ->
 ToggleButtonList.Orientation ->
 [String] ->
 IO (GTK.Box,ToggleButtonListUpdater)-}

toggleButtonListNew
 onToggle
 orientation
 drawHelper
 labels
  = do
 b <- case orientation of
  Horizontal -> do
   hb <- GTK.hBoxNew False 0
   return $ castToBox hb
  Vertical -> do
   vb <- GTK.vBoxNew False 0
   return $ castToBox vb
 let
  addToggleButton label = do
   tb <- GTK.toggleButtonNewWithLabel label
   GTK.boxPackStart b tb GTK.PackNatural 0
   return tb
  clearBox = do
   GTK.containerForall b GTK.widgetDestroy
  fillBox labels = do
   tbs<-mapM addToggleButton labels
   mapM drawHelper tbs
   GTK.widgetShowAll b
   return tbs
 tbs <- fillBox labels
 let
  tbWithLabels tbs labels = zip tbs labels
  setupToggleButton
   labels'
   tbs
   (tb,label)
    =
   tb `on` GTK.toggled $ do
    active <- get tb GTK.toggleButtonActive
    case active of
     True -> do
      onToggle label
      mapM_
       (\(tb',label') ->
        case label == label' of
         True  -> return ()
         False -> set tb' [GTK.toggleButtonActive := False])
       (tbWithLabels tbs labels')
     False-> return ()
  updateToggleButtons :: [String] -> IO()
  updateToggleButtons labels = do
    clearBox
    tbs <- fillBox labels
    mapM_ (setupToggleButton labels tbs) (tbWithLabels tbs labels)

 mapM (setupToggleButton labels tbs) (tbWithLabels tbs labels)
 return (b,updateToggleButtons)
