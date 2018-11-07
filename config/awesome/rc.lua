-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, 'luarocks.loader')

-- set the package paths
local gfs = require('gears.filesystem')
package.path = gfs.get_configuration_dir() .. '/lib/?.lua;' .. package.path
package.path = gfs.get_configuration_dir() .. '/lib/?/init.lua;' .. package.path

require('awful.autofocus')
require('awful.hotkeys_popup.keys')

local freedesktop = require('freedesktop')
local AwesomeApi = require('AwesomeApi')
local TagList = require('TagList')
local configs = require('config')


-- Awesome API
local awesomeApi = AwesomeApi(_ENV)

awesomeApi:configure()

-- Globals to locals
local os = os
local type = type

-- from now on, no globals
_ENV = nil

-- This is used later as the default terminal and editor to run.
local terminal   = configs.terminal or 'xterm'
local editor     = configs.editor or os.getenv('EDITOR') or 'vim'
local editor_cmd = configs.editorCmd or terminal .. ' -e ' .. editor
local gui_editor = configs.editorGui or 'gvim'
local browser    = configs.browser or 'firefox'
local modkey     = configs.modkey or 'Mod4'

awesomeApi.beautiful.init(awesomeApi.gears.filesystem.get_configuration_dir() .. '/theme.lua')


-- Table of layouts to cover with awesomeApi.awful.layout.inc, order matters.
awesomeApi.awful.layout.layouts = {
    -- awesomeApi.awful.layout.suit.floating,
    awesomeApi.awful.layout.suit.tile.bottom,
    awesomeApi.awful.layout.suit.tile,
    awesomeApi.awful.layout.suit.tile.left,
    awesomeApi.awful.layout.suit.tile.top,
    awesomeApi.awful.layout.suit.fair,
    awesomeApi.awful.layout.suit.fair.horizontal,
    awesomeApi.awful.layout.suit.spiral,
    awesomeApi.awful.layout.suit.spiral.dwindle,
    awesomeApi.awful.layout.suit.max,
    awesomeApi.awful.layout.suit.max.fullscreen,
    awesomeApi.awful.layout.suit.magnifier,
    awesomeApi.awful.layout.suit.corner.nw,
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.awesomeApi.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awesomeApi.awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
local myawesomemenu = {
   { 'hotkeys', function() return false, hotkeysPopup.show_help end},
   { 'manual', terminal .. ' -e man awesome' },
   { 'edit config', editor_cmd .. ' ' .. awesomeApi.conffile },
   { 'restart', awesomeApi.restart },
   { 'quit', function() awesomeApi.quit() end}
}

local mymainmenu = freedesktop.menu.build({
    before = {
      { 'Awesome', myawesomemenu, awesomeApi.beautiful.awesome_icon },
    },

    after = {
      { 'open terminal', terminal },
    }
})

local mylauncher = awesomeApi.awful.widget.launcher({
    image = awesomeApi.beautiful.awesome_icon,
    menu = mymainmenu,
    shape = awesomeApi.gears.shape.rounded_rect,
    shape_clip = true,
})


-- Keyboard map indicator and switcher
local mykeyboardlayout = awesomeApi.awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
local mytextclock = awesomeApi.wibox.widget.textclock()


local tasklist_buttons = awesomeApi.gears.table.join(
                     awesomeApi.awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      'request::activate',
                                                      'tasklist',
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awesomeApi.awful.button({ }, 3, client_menu_toggle_fn()),
                     awesomeApi.awful.button({ }, 4, function ()
                                              awesomeApi.awful.client.focus.byidx(1)
                                          end),
                     awesomeApi.awful.button({ }, 5, function ()
                                              awesomeApi.awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if awesomeApi.beautiful.wallpaper then
        local wallpaper = awesomeApi.beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == 'function' then
            wallpaper = wallpaper(s)
        end
        awesomeApi.gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
awesomeApi.screens.connect_signal('property::geometry', set_wallpaper)

awesomeApi.awful.screen.connect_for_each_screen(function(s)
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awesomeApi.awful.tag({ 'WEB', 'CODE', 'SHELL', 'OFFICE', 'GRAPHICS', 'FILES', 'MISC' }, s, awesomeApi.awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awesomeApi.awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per awesomeApi.screens.
    s.mylayoutbox = awesomeApi.awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awesomeApi.gears.table.join(
                           awesomeApi.awful.button({ }, 1, function () awesomeApi.awful.layout.inc( 1) end),
                           awesomeApi.awful.button({ }, 3, function () awesomeApi.awful.layout.inc(-1) end),
                           awesomeApi.awful.button({ }, 4, function () awesomeApi.awful.layout.inc( 1) end),
                           awesomeApi.awful.button({ }, 5, function () awesomeApi.awful.layout.inc(-1) end)))
    -- Create a taglist widget
    -- s.mytaglist = awesomeApi.awful.widget.taglist {
    --     screen  = s,
    --     filter  = awesomeApi.awful.widget.taglist.filter.all,
    --     buttons = taglist_buttons
    -- }

    s.mytaglist = TagList(s, awesomeApi, configs)

    -- Create a tasklist widget
    s.mytasklist = awesomeApi.awful.widget.tasklist {
        screen  = s,
        filter  = awesomeApi.awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awesomeApi.awful.wibar({ position = 'top', screen = s, bg = '#00000000' })

    -- Add widgets to the wibox
    s.mywibox:setup {
      {
        {
          layout = awesomeApi.wibox.layout.align.horizontal,
          { -- Left widgets
            layout = awesomeApi.wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
          },
          s.mytasklist, -- Middle widget
          { -- Right widgets
            layout = awesomeApi.wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            awesomeApi.wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
          },
        },
        widget = awesomeApi.wibox.container.background,
        bg = awesomeApi.beautiful.bg_normal,
        shape = awesomeApi.gears.shape.rounded_rect,
        shape_clip = true,
      },
      widget = awesomeApi.wibox.container.margin,
      top = 8, left = 8, right = 8, bottom = 0,
    }
end)
-- }}}

-- {{{ Mouse bindings
awesomeApi.rootWindow.buttons(awesomeApi.gears.table.join(
    awesomeApi.awful.button({ }, 3, function () mymainmenu:toggle() end),
    awesomeApi.awful.button({ }, 4, awesomeApi.awful.tag.viewnext),
    awesomeApi.awful.button({ }, 5, awesomeApi.awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
local globalkeys = awesomeApi.gears.table.join(
    -- awesome
    awesomeApi.awful.key({ modkey,           }, 's',      awesomeApi.hotkeysPopup.show_help,
              {description='show help', group='awesome'}),
    awesomeApi.awful.key({ modkey,           }, 'w', function () mymainmenu:show() end,
      {description = 'show main menu', group = 'awesome'}),
    awesomeApi.awful.key({ modkey, 'Control' }, 'r', awesomeApi.restart,
      {description = 'reload awesome', group = 'awesome'}),
    awesomeApi.awful.key({ modkey, 'Shift'   }, 'q', awesomeApi.quit,
      {description = 'quit awesome', group = 'awesome'}),

    -- tag
    awesomeApi.awful.key({ modkey,           }, 'Left',   awesomeApi.awful.tag.viewprev,
              {description = 'view previous', group = 'tag'}),
    awesomeApi.awful.key({ modkey,           }, 'Right',  awesomeApi.awful.tag.viewnext,
              {description = 'view next', group = 'tag'}),
    awesomeApi.awful.key({ modkey,           }, 'Escape', awesomeApi.awful.tag.history.restore,
              {description = 'go back', group = 'tag'}),

    -- client
    awesomeApi.awful.key({ modkey,           }, 'j',
        function ()
            awesomeApi.awful.client.focus.byidx( 1)
        end,
        {description = 'focus next by index', group = 'client'}
    ),
    awesomeApi.awful.key({ modkey,           }, 'k',
        function ()
            awesomeApi.awful.client.focus.byidx(-1)
        end,
        {description = 'focus previous by index', group = 'client'}
    ),
    awesomeApi.awful.key({ modkey, 'Shift'   }, 'j', function () awesomeApi.awful.client.swap.byidx(  1)    end,
      {description = 'swap with next client by index', group = 'client'}),
    awesomeApi.awful.key({ modkey, 'Shift'   }, 'k', function () awesomeApi.awful.client.swap.byidx( -1)    end,
      {description = 'swap with previous client by index', group = 'client'}),
    awesomeApi.awful.key({ modkey,           }, 'u', awesomeApi.awful.client.urgent.jumpto,
      {description = 'jump to urgent client', group = 'client'}),
    awesomeApi.awful.key({ modkey,           }, 'Tab',
      function ()
        awesomeApi.awful.client.focus.history.previous()
        if client.focus then
          client.focus:raise()
        end
      end,
      {description = 'go back', group = 'client'}),

    -- screen
    awesomeApi.awful.key({ modkey, 'Control' }, 'j', function () awesomeApi.awful.focus_relative( 1) end,
              {description = 'focus the next screen', group = 'screen'}),
    awesomeApi.awful.key({ modkey, 'Control' }, 'k', function () awesomeApi.awful.focus_relative(-1) end,
              {description = 'focus the previous screen', group = 'screen'}),

    -- launcher
    awesomeApi.awful.key({ modkey,           }, 'Return', function () awesomeApi.awful.spawn(terminal) end,
              {description = 'open a terminal', group = 'launcher'}),
    awesomeApi.awful.key({ modkey,           }, 'a', function () awesomeApi.awful.spawn(gui_editor) end,
              {description = 'open the editor', group = 'launcher'}),
    awesomeApi.awful.key({ modkey,           }, 'q', function () awesomeApi.awful.spawn(browser) end,
              {description = 'open the broswer', group = 'launcher'}),

    -- layout
    awesomeApi.awful.key({ modkey,           }, 'l',     function () awesomeApi.awful.tag.incmwfact( 0.05)          end,
              {description = 'increase master width factor', group = 'layout'}),
    awesomeApi.awful.key({ modkey,           }, 'h',     function () awesomeApi.awful.tag.incmwfact(-0.05)          end,
              {description = 'decrease master width factor', group = 'layout'}),
    awesomeApi.awful.key({ modkey, 'Shift'   }, 'h',     function () awesomeApi.awful.tag.incnmaster( 1, nil, true) end,
              {description = 'increase the number of master clients', group = 'layout'}),
    awesomeApi.awful.key({ modkey, 'Shift'   }, 'l',     function () awesomeApi.awful.tag.incnmaster(-1, nil, true) end,
              {description = 'decrease the number of master clients', group = 'layout'}),
    awesomeApi.awful.key({ modkey, 'Control' }, 'h',     function () awesomeApi.awful.tag.incncol( 1, nil, true)    end,
              {description = 'increase the number of columns', group = 'layout'}),
    awesomeApi.awful.key({ modkey, 'Control' }, 'l',     function () awesomeApi.awful.tag.incncol(-1, nil, true)    end,
              {description = 'decrease the number of columns', group = 'layout'}),
    awesomeApi.awful.key({ modkey,           }, 'space', function () awesomeApi.awful.layout.inc( 1)                end,
              {description = 'select next', group = 'layout'}),
    awesomeApi.awful.key({ modkey, 'Shift'   }, 'space', function () awesomeApi.awful.layout.inc(-1)                end,
              {description = 'select previous', group = 'layout'}),

    awesomeApi.awful.key({ modkey, 'Control' }, 'n',
              function ()
                  local c = awesomeApi.awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        'request::activate', 'key.unminimize', {raise = true}
                    )
                  end
              end,
              {description = 'restore minimized', group = 'client'}),

    -- Prompt
    awesomeApi.awful.key({ modkey },            'r',     function () awesomeApi.awful.focused().mypromptbox:run() end,
              {description = 'run prompt', group = 'launcher'}),

    awesomeApi.awful.key({ modkey }, 'x',
              function ()
                  awesomeApi.awful.prompt.run {
                    prompt       = 'Run Lua code: ',
                    textbox      = awesomeApi.awful.focused().mypromptbox.widget,
                    exe_callback = awesomeApi.awful.util.eval,
                    history_path = awesomeApi.awful.util.get_cache_dir() .. '/history_eval'
                  }
              end,
              {description = 'lua execute prompt', group = 'awesome'}),
    -- Menubar
    awesomeApi.awful.key({ modkey }, 'p', function() menubar.show() end,
              {description = 'show the menubar', group = 'launcher'})
)

local clientkeys = awesomeApi.gears.table.join(
    awesomeApi.awful.key({ modkey,           }, 'f',
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = 'toggle fullscreen', group = 'client'}),
    awesomeApi.awful.key({ modkey, 'Shift'   }, 'c',      function (c) c:kill()                         end,
              {description = 'close', group = 'client'}),
    awesomeApi.awful.key({ modkey, 'Control' }, 'space',  awesomeApi.awful.client.floating.toggle                     ,
              {description = 'toggle floating', group = 'client'}),
    awesomeApi.awful.key({ modkey, 'Control' }, 'Return', function (c) c:swap(awesomeApi.awful.client.getmaster()) end,
              {description = 'move to master', group = 'client'}),
    awesomeApi.awful.key({ modkey,           }, 'o',      function (c) c:move_to_screen()               end,
              {description = 'move to screen', group = 'client'}),
    awesomeApi.awful.key({ modkey,           }, 't',      function (c) c.ontop = not c.ontop            end,
              {description = 'toggle keep on top', group = 'client'}),
    awesomeApi.awful.key({ modkey,           }, 'n',
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = 'minimize', group = 'client'}),
    awesomeApi.awful.key({ modkey,           }, 'm',
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = '(un)maximize', group = 'client'}),
    awesomeApi.awful.key({ modkey, 'Control' }, 'm',
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = '(un)maximize vertically', group = 'client'}),
    awesomeApi.awful.key({ modkey, 'Shift'   }, 'm',
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        {description = '(un)maximize horizontally', group = 'client'})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awesomeApi.gears.table.join(globalkeys,
        -- View tag only.
        awesomeApi.awful.key({ modkey }, '#' .. i + 9,
                  function ()
                        local screen = awesomeApi.awful.focused()
                        local tag = awesomeApi.screens.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = 'view tag #'..i, group = 'tag'}),
        -- Toggle tag display.
        awesomeApi.awful.key({ modkey, 'Control' }, '#' .. i + 9,
                  function ()
                      local screen = awesomeApi.awful.focused()
                      local tag = awesomeApi.screens.tags[i]
                      if tag then
                         awesomeApi.awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = 'toggle tag #' .. i, group = 'tag'}),
        -- Move client to tag.
        awesomeApi.awful.key({ modkey, 'Shift' }, '#' .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = 'move focused client to tag #'..i, group = 'tag'}),
        -- Toggle tag on focused client.
        awesomeApi.awful.key({ modkey, 'Control', 'Shift' }, '#' .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = 'toggle focused client on tag #' .. i, group = 'tag'})
    )
end

local clientbuttons = awesomeApi.gears.table.join(
    awesomeApi.awful.button({ }, 1, function (c)
        c:emit_signal('request::activate', 'mouse_click', {raise = true})
    end),
    awesomeApi.awful.button({ modkey }, 1, function (c)
        c:emit_signal('request::activate', 'mouse_click', {raise = true})
        awesomeApi.awful.mouse.client.move(c)
    end),
    awesomeApi.awful.button({ modkey }, 3, function (c)
        c:emit_signal('request::activate', 'mouse_click', {raise = true})
        awesomeApi.awful.mouse.client.resize(c)
    end)
)

-- Set keys
awesomeApi.rootWindow.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the 'manage' signal).
awesomeApi.awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = awesomeApi.beautiful.border_width,
                     border_color = awesomeApi.beautiful.border_normal,
                     focus = awesomeApi.awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awesomeApi.awful.screen.preferred,
                     placement = awesomeApi.awful.placement.no_overlap+awesomeApi.awful.placement.no_offscreen,
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          'DTA',  -- Firefox addon DownThemAll.
          'copyq',  -- Includes session name in class.
          'pinentry',
        },
        class = {
          'Arandr',
          'Blueman-manager',
          'Gpick',
          'Kruler',
          'MessageWin',  -- kalarm.
          'Sxiv',
          'Wpa_gui',
          'veromix',
          'xtightvncviewer'},

        name = {
          'Event Tester',  -- xev.
        },
        role = {
          'AlarmWindow',  -- Thunderbird's calendar.
          'ConfigManager',  -- Thunderbird's about:config.
          'pop-up',       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { 'normal', 'dialog' }
      }, properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named '2' on screen 1.
    { rule = { class = 'Emacs' },
      properties = { tag = 'CODE' } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
awesomeApi.clients.connect_signal('manage', function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesomeApi.startup then awesomeApi.awful.client.setslave(c) end


    if awesomeApi.startup
        and not c.size_hints.user_position
        and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awesomeApi.awful.placement.no_offscreen(c)
    end

    c.shape = awesomeApi.gears.shape.rounded_rect
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
awesomeApi.clients.connect_signal('request::titlebars', function(c)
    -- buttons for the titlebar
    local buttons = awesomeApi.gears.table.join(
        awesomeApi.awful.button({ }, 1, function()
            c:emit_signal('request::activate', 'titlebar', {raise = true})
            awesomeApi.awful.mouse.client.move(c)
        end),
        awesomeApi.awful.button({ }, 3, function()
            c:emit_signal('request::activate', 'titlebar', {raise = true})
            awesomeApi.awful.mouse.client.resize(c)
        end)
    )

    awesomeApi.awful.titlebar(c) : setup {
        { -- Left
            awesomeApi.awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = awesomeApi.wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = 'center',
                widget = awesomeApi.awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = awesomeApi.wibox.layout.flex.horizontal
        },
        { -- Right
            awesomeApi.awful.titlebar.widget.floatingbutton (c),
            awesomeApi.awful.titlebar.widget.maximizedbutton(c),
            awesomeApi.awful.titlebar.widget.stickybutton   (c),
            awesomeApi.awful.titlebar.widget.ontopbutton    (c),
            awesomeApi.awful.titlebar.widget.closebutton    (c),
            layout = awesomeApi.wibox.layout.fixed.horizontal()
        },
        layout = awesomeApi.wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
awesomeApi.clients.connect_signal('mouse::enter', function(c)
    c:emit_signal('request::activate', 'mouse_enter', {raise = true})
end)
-- }}}
