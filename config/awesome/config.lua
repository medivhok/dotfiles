local editor = os.getenv('EDITOR') or 'vim';
local terminal = 'termite'
local screens = {}

screens[1] = 2
screens[2] = 1
screens[3] = 3


return {
   browser   = 'chromium',
   editor    = editor,
   editorCmd = terminal .. ' -e ' .. editor,
   editorGui = 'emacs',
   modkey    = 'Mod4',
   editor    = editor,
   editorCmd = editorCmd,
   editorGui = editorGui,
   modkey    = 'Mod4',
   screens   = screens,
   terminal  = terminal,
}
