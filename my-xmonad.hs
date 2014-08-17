import XMonad
import Data.Default (def)
import System.Environment (lookupEnv)
import XMonad.Layout.Decoration (Theme (..), DefaultShrinker(..))
import XMonad.Util.EZConfig (mkKeymap, checkKeymap, additionalKeys)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Groups.Helpers (moveToGroupUp, moveToGroupDown, swapUp
                                    ,swapDown, swapMaster, focusGroupUp
                                    ,focusGroupDown, focusUp, focusDown)
import XMonad.Layout.Tabbed (Shrinker(..), addTabs)
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.Groups (group)
import XMonad.Layout.Groups.Examples (TiledTabsConfig(..)
                                     ,tallTabs
                                     ,rowOfColumns, shrinkMasterGroups
                                     ,expandMasterGroups
                                     ,increaseNMasterGroups
                                     ,decreaseNMasterGroups
                                     ,shrinkText)
import XMonad.Actions.CycleWS (WSType(AnyWS))
import XMonad.Util.Types (Direction1D(Next, Prev))
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat
                                  ,composeOne, (-?>))
import XMonad.Hooks.ManageDocks (Direction2D(L, R, U, D)
                                ,ToggleStruts(..)
                                ,manageDocks, avoidStruts)
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.WindowGo (runOrRaiseNext, raiseNextMaybe)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt
                                        , removeEmptyWorkspace
                                        , withWorkspace
                                        , selectWorkspace
                                        , toNthWorkspace)
import XMonad.Actions.DynamicWorkspaceOrder ( moveTo
                                            , shiftTo
                                            , withNthWorkspace
                                            , swapWith
                                            , getSortByOrder
                                            )
import XMonad.Actions.WorkspaceNames ( renameWorkspace
                                     )
import XMonad.Layout.Named (named)
import XMonad.Layout.Renamed (renamed, Rename(..))
import XMonad.Actions.Navigation2D (Navigation2D, Direction2D
                                   ,lineNavigation, centerNavigation
                                   ,fullScreenRect, singleWindowRect
                                   ,switchLayer, windowGo, windowSwap
                                   ,windowToScreen, screenGo, screenSwap)
import XMonad.Hooks.DynamicLog (PP, dynamicLogString, dynamicLogWithPP
                               ,pad, ppTitle, ppLayout
                               ,ppCurrent, ppVisible, ppHidden
                               ,ppHiddenNoWindows, ppUrgent, ppSep
                               ,ppOutput, ppWsSep, ppExtras, wrap, shorten
                               ,xmobarColor, ppSort)
import XMonad.Actions.TagWindows (addTag, tagDelPrompt, tagPrompt)
import XMonad.Actions.CycleWS ( toggleWS )
import XMonad.Actions.GridSelect (goToSelected)
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.Loggers (Logger
                           ,logCmd
                           ,loadAvg
                           ,date
                           ,battery)
import XMonad.Prompt (XPConfig (..)
                     ,XPPosition(Top, Bottom)
                     ,font
                     ,bgColor
                     ,defaultXPKeymap
                     ,fgColor
                     ,fgHLight
                     ,bgHLight
                     ,borderColor
                     ,promptBorderWidth
                     ,promptKeymap
                     ,completionKey
                     ,changeModeKey
                     ,position
                     ,height
                     ,historySize
                     ,historyFilter
                     ,defaultText
                     ,autoComplete
                     ,showCompletionOnTab
                     ,searchPredicate
                     ,alwaysHighlight
                     )
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (windowPromptGoto)
import XMonad.StackSet (shiftMaster, focusMaster, sink
                       ,greedyView, shift, view)
import Data.List (isPrefixOf)
import Data.Monoid (All, mempty)
import Data.Map (Map, fromList)
import System.Process (CmdSpec (RawCommand))
import System.Exit (exitSuccess)

colorBackground :: String
colorBackground = "#151515"

colorCurrentLine :: String
colorCurrentLine = "#B8D6AC"

colorSelection :: String
colorSelection = "#404040"

colorForeground :: String
colorForeground = "#D7D0C7"

colorComment :: String
colorComment = "#dddddd"

colorRed :: String
colorRed = "#E84F4F"

colorOrange :: String
colorOrange = "#F39D21"

colorYellow :: String
colorYellow = "#E1AA5D"

colorGreen :: String
colorGreen = "#B8D6AC"

colorAqua :: String
colorAqua = "#4E9FB1"

colorBlue :: String
colorBlue = "#7DC1CF"

colorPurple :: String
colorPurple = "#9B64FB"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth   = 1

myWorkspaces :: [String]
myWorkspaces = ["home"]

myModMask :: KeyMask
myModMask = mod4Mask

myNormalBorderColor :: String
myNormalBorderColor = colorBackground

myFocusedBorderColor :: String
myFocusedBorderColor = colorSelection

terminus :: String
terminus = "-*-terminus-medium-r-*-*-12-120-*-*-*-*-iso8859-*"

shiftLayout :: X ()
shiftLayout =
    sendMessage NextLayout

myKeymap =
     [ ("M-S-r", renameWorkspace myXPConfig)
     , ("M-<Return>", spawn "urxvt")
     , ("M-S-c", kill)
     , ("M-<Space>", shiftLayout)
     , ("M-n", refresh)
     , ("M-h", windowGo   L False)
     , ("M-j", focusGroupDown)
     , ("M-k", focusGroupUp)
     , ("M-l", windowGo   R False)
     , ("M-<Tab>", focusDown)
     , ("M-`", raiseNextMaybe (return ()) (className =? "emacs"))
     , ("M-S-`", spawn "~/bin/emc")
     , ("M-S-<Tab>", focusUp)
     , ("M-S-h", moveToGroupUp False)
     , ("M-S-j", focusDown)
     , ("M-S-k", focusUp)
     , ("M-S-l", moveToGroupDown False)
     , ("M-C-h", shrinkMasterGroups)
     , ("M-C-j", increaseNMasterGroups)
     , ("M-C-k", decreaseNMasterGroups)
     , ("M-C-l", expandMasterGroups)
     , ("M-s", goToSelected def)
     , ("M-f", withFocused $ windows . sink)
     , ("M-,", sendMessage (IncMasterN 1))
     , ("M-.", sendMessage (IncMasterN (-1)))
     , ("M-b", sendMessage ToggleStruts)
     , ("M-C-q", io exitSuccess)
     , ("M-i", runOrRaiseNext "firefox" (className =? "Firefox"))
     , ("M-t", spawn "~/bin/tmux-urxvt")
     , ("M-o", toggleWS)
     , ("M-p", shellPrompt myXPConfig)
     , ("M-/", windowPromptGoto myXPConfig {autoComplete = Just 500000})
     , ("M m", tagPrompt myXPConfig $ withFocused . addTag)
     , ("M-S m", tagDelPrompt myXPConfig)
     , ("M-v", selectWorkspace myXPConfig)
     , ("M-C-m", withWorkspace myXPConfig (windows . copy))
     , ("M-S-<Backspace>", removeEmptyWorkspace)
     , ("M-S-a", addWorkspacePrompt myXPConfig)
     , ("M-<R>", moveTo Next AnyWS)
     , ("M-<L>", moveTo Prev AnyWS)
     , ("M-S-<R>", swapWith Next AnyWS)
     , ("M-S-<L>", swapWith Prev AnyWS)
     ]


myMouseBindings :: XConfig t -> Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w  >> mouseMoveWindow w
                                       >> windows shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows shiftMaster)
    ]

myTheme :: Theme
myTheme = def { activeColor         = colorSelection
              , inactiveColor       = colorBackground
              , urgentColor         = colorRed
              , activeBorderColor   = colorSelection
              , inactiveBorderColor = colorBackground
              , urgentBorderColor   = colorBackground
              , activeTextColor     = colorGreen
              , inactiveTextColor   = colorComment
              , urgentTextColor     = "#FF0000"
              , fontName            = terminus
              , decoWidth           = 200
              , decoHeight          = 16
             , windowTitleAddons   = []
              , windowTitleIcons    = []
              }

myLayout = avoidStruts $
    tallTabs (TTC 1 0.5 (3/100) 1 0.5 (3/100) shrinkText myTheme)
    ||| smartBorders tiled
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio

        -- The default number of windows in the master pane
        nmaster = 1

        -- Default proportion of screen occupied by master pane
        ratio   = 3/5

        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

myManageHook :: ManageHook
myManageHook = manageDocks
    <+> composeAll
        [ className =? "MPlayer"        --> doFloat
        , className =? "Gimp"           --> doFloat
        , stringProperty "WM_NAME" =? "Firefox Preferences" --> doFloat
        -- Float Firefox dialog windows
        , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
        , resource  =? "desktop_window" --> doIgnore
        ]
    <+>
        composeOne [isFullscreen -?> doFullFloat]

myEventHook :: Event -> X All
myEventHook = mempty

myLogHook :: X ()
myLogHook = ewmhDesktopsLogHook

myStartupHook :: X ()
myStartupHook = return ()

myBottomXPConfig :: XPConfig
myBottomXPConfig = XPC {
                         XMonad.Prompt.font = terminus
                       , bgColor            = colorBackground
                       , fgColor            = colorForeground
                       , fgHLight           = colorForeground
                       , bgHLight           = colorSelection
                       , borderColor        = colorComment
                       , promptBorderWidth  = 0
                       , promptKeymap       = defaultXPKeymap
                       , completionKey      = xK_Tab
                       , changeModeKey      = xK_grave
                       , maxComplRows       = Just 10
                       , position           = Bottom
                       , height             = 16
                       , historySize        = 256
                       , historyFilter      = id
                       , defaultText        = []
                       , autoComplete       = Nothing
                       , showCompletionOnTab = False
                       , searchPredicate    = isPrefixOf
                       , alwaysHighlight    = True
                       }

myXPConfig :: XPConfig
myXPConfig = XPC {
          XMonad.Prompt.font = terminus
        , bgColor            = colorBackground
        , fgColor            = colorForeground
        , fgHLight           = colorForeground
        , bgHLight           = colorSelection
        , borderColor        = colorComment
        , promptBorderWidth  = 0
        , promptKeymap       = defaultXPKeymap
        , completionKey      = xK_Tab
        , changeModeKey      = xK_grave
        , maxComplRows       = Just 10
        , position           = Top
        , height             = 16
        , historySize        = 256
        , historyFilter      = id
        , defaultText        = []
        , autoComplete       = Nothing
        , showCompletionOnTab = False
        , searchPredicate    = isPrefixOf
        , alwaysHighlight    = True
        }

-- | Some nice xmobar defaults.
mybarPP :: PP
mybarPP = def { ppCurrent          = xmobarColor colorBackground colorGreen . wrap "[" "]"
              , ppTitle            = xmobarColor colorGreen  colorSelection . shorten 100
              , ppHidden           = xmobarColor colorBlue   colorSelection
              , ppHiddenNoWindows  = xmobarColor colorAqua   colorSelection
              , ppSort = getSortByOrder
              , ppUrgent           = xmobarColor colorRed colorYellow
              }

myExtraKeys =
        -- mod-[1..9]       %! Switch to workspace N
        -- mod-shift-[1..9] %! Move client to workspace N
        zip (zip (repeat (myModMask)) [xK_1..xK_9]) (map (withNthWorkspace greedyView) [0..])
        ++
        zip (zip (repeat (myModMask .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace shift) [0..])
        ++
        --
        -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
        -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
        --
        [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
            , (f, m) <- [(view, 0), (shift, shiftMask)]]

myConfig = def {
            focusFollowsMouse  = myFocusFollowsMouse,
            borderWidth        = myBorderWidth,
            modMask            = myModMask,
            workspaces         = myWorkspaces,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
            keys               = \c -> mkKeymap c myKeymap,
            mouseBindings      = myMouseBindings,
            layoutHook         = myLayout,
            manageHook         = myManageHook,
            handleEventHook    = myEventHook,
            startupHook        = return () >> checkKeymap myConfig myKeymap
        } `additionalKeys` myExtraKeys

main :: IO ()
main = do
    h <- spawnPipe "/home/nathan/.xmonad/xmobar/dist/build/xmobar/xmobar"
    xmonad myConfig {
        logHook = ewmhDesktopsLogHook <+> dynamicLogWithPP mybarPP { ppOutput = hPutStrLn h }
      }
