-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, 'luarocks.loader')

require('awful.autofocus')
require('awful.hotkeys_popup.keys')


local freedesktop = require('freedesktop')
local AwesomeApi = require('AwesomeApi')
local TagList = require('TagList')
local configs = require('config')


-- Awesome API
AwesomeApi(_ENV, {
    awful     = require('awful'),
    beaufiful = require('beautiful'),
    gears     = require('gears'),
    naughty   = require('naughty'),
    wibox     = require('wibox'),
})

AwesomeApi:configure()

-- Globals to locals
local os = os
local type = type

-- from now on, no globals
_ENV = nil

-- This is used later as the default terminal and editor to run.
local terminal   = 'termite'
local editor     = os.getenv('EDITOR') or 'vim'
local editor_cmd = terminal .. ' -e ' .. editor
local gui_editor = 'emacs'
local browser    = 'chromium'
local modkey     = 'Mod4'

AwesomeApi.beautiful.init(AwesomeApi.gears.filesystem.get_configuration_dir() .. '/theme.lua')


-- Table of layouts to cover with AwesomeApi.awful.layout.inc, order matters.
AwesomeApi.awful.layout.layouts = {
    -- AwesomeApi.awful.layout.suit.floating,
    AwesomeApi.awful.layout.suit.tile.bottom,
    AwesomeApi.awful.layout.suit.tile,
    AwesomeApi.awful.layout.suit.tile.left,
    AwesomeApi.awful.layout.suit.tile.top,
    AwesomeApi.awful.layout.suit.fair,
    AwesomeApi.awful.layout.suit.fair.horizontal,
    AwesomeApi.awful.layout.suit.spiral,
    AwesomeApi.awful.layout.suit.spiral.dwindle,
    AwesomeApi.awful.layout.suit.max,
    AwesomeApi.awful.layout.suit.max.fullscreen,
    AwesomeApi.awful.layout.suit.magnifier,
    AwesomeApi.awful.layout.suit.corner.nw,
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.AwesomeApi.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = AwesomeApi.awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
local myawesomemenu = {
   { 'hotkeys', function() return false, hotkeysPopup.show_help end},
   { 'manual', terminal .. ' -e man awesome' },
   { 'edit config', editor_cmd .. ' ' .. AwesomeApi.conffile },
   { 'restart', AwesomeApi.restart },
   { 'quit', function() AwesomeApi.quit() end}
}

local mymainmenu = freedesktop.menu.build({
    before = {
      { 'Awesome', myawesomemenu, AwesomeApi.beautiful.awesome_icon },
    },

    after = {
      { 'open terminal', terminal },
    }
})

local mylauncher = AwesomeApi.awful.widget.launcher({
    image = AwesomeApi.beautiful.awesome_icon,
    menu = mymainmenu,
    shape = AwesomeApi.gears.shape.rounded_rect,
    shape_clip = true,
})


-- Keyboard map indicator and switcher
local mykeyboardlayout = AwesomeApi.awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
local mytextclock = AwesomeApi.wibox.widget.textclock()


local tasklist_buttons = AwesomeApi.gears.table.join(
                     AwesomeApi.awful.button({ }, 1, function (c)
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
                     AwesomeApi.awful.button({ }, 3, client_menu_toggle_fn()),
                     AwesomeApi.awful.button({ }, 4, function ()
                                              AwesomeApi.awful.client.focus.byidx(1)
                                          end),
                     AwesomeApi.awful.button({ }, 5, function ()
                                              AwesomeApi.awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if AwesomeApi.beautiful.wallpaper then
        local wallpaper = AwesomeApi.beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == 'function' then
            wallpaper = wallpaper(s)
        end
        AwesomeApi.gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
AwesomeApi.screens.connect_signal('property::geometry', set_wallpaper)

AwesomeApi.awful.screen.connect_for_each_screen(function(s)
    set_wallpaper(s)

    -- Each screen has its own tag table.
    AwesomeApi.awful.tag({ 'WEB', 'CODE', 'SHELL', 'OFFICE', 'GRAPHICS', 'FILES', 'MISC' }, s, AwesomeApi.awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = AwesomeApi.awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per AwesomeApi.screens.
    s.mylayoutbox = AwesomeApi.awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(AwesomeApi.gears.table.join(
                           AwesomeApi.awful.button({ }, 1, function () AwesomeApi.awful.layout.inc( 1) end),
                           AwesomeApi.awful.button({ }, 3, function () AwesomeApi.awful.layout.inc(-1) end),
                           AwesomeApi.awful.button({ }, 4, function () AwesomeApi.awful.layout.inc( 1) end),
                           AwesomeApi.awful.button({ }, 5, function () AwesomeApi.awful.layout.inc(-1) end)))
    -- Create a taglist widget
    -- s.mytaglist = AwesomeApi.awful.widget.taglist {
    --     screen  = s,
    --     filter  = AwesomeApi.awful.widget.taglist.filter.all,
    --     buttons = taglist_buttons
    -- }

    s.mytaglist = TagList(s, AwesomeApi, configs)

    -- Create a tasklist widget
    s.mytasklist = AwesomeApi.awful.widget.tasklist {
        screen  = s,
        filter  = AwesomeApi.awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = AwesomeApi.awful.wibar({ position = 'top', screen = s, bg = '#00000000' })

    -- Add widgets to the wibox
    s.mywibox:setup {
      {
        {
          layout = AwesomeApi.wibox.layout.align.horizontal,
          { -- Left widgets
            layout = AwesomeApi.wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
          },
          s.mytasklist, -- Middle widget
          { -- Right widgets
            layout = AwesomeApi.wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            AwesomeApi.wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
          },
        },
        widget = AwesomeApi.wibox.container.background,
        bg = AwesomeApi.beautiful.bg_normal,
        shape = AwesomeApi.gears.shape.rounded_rect,
        shape_clip = true,
      },
      widget = AwesomeApi.wibox.container.margin,
      top = 8, left = 8, right = 8, bottom = 0,
    }
end)
-- }}}

-- {{{ Mouse bindings
AwesomeApi.rootWindow.buttons(AwesomeApi.gears.table.join(
    AwesomeApi.awful.button({ }, 3, function () mymainmenu:toggle() end),
    AwesomeApi.awful.button({ }, 4, AwesomeApi.awful.tag.viewnext),
    AwesomeApi.awful.button({ }, 5, AwesomeApi.awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
local globalkeys = AwesomeApi.gears.table.join(
    -- awesome
    AwesomeApi.awful.key({ modkey,           }, 's',      AwesomeApi.hotkeysPopup.show_help,
              {description='show help', group='awesome'}),
    AwesomeApi.awful.key({ modkey,           }, 'w', function () mymainmenu:show() end,
      {description = 'show main menu', group = 'awesome'}),
    AwesomeApi.awful.key({ modkey, 'Control' }, 'r', AwesomeApi.restart,
      {description = 'reload awesome', group = 'awesome'}),
    AwesomeApi.awful.key({ modkey, 'Shift'   }, 'q', AwesomeApi.quit,
      {description = 'quit awesome', group = 'awesome'}),

    -- tag
    AwesomeApi.awful.key({ modkey,           }, 'Left',   AwesomeApi.awful.tag.viewprev,
              {description = 'view previous', group = 'tag'}),
    AwesomeApi.awful.key({ modkey,           }, 'Right',  AwesomeApi.awful.tag.viewnext,
              {description = 'view next', group = 'tag'}),
    AwesomeApi.awful.key({ modkey,           }, 'Escape', AwesomeApi.awful.tag.history.restore,
              {description = 'go back', group = 'tag'}),

    -- client
    AwesomeApi.awful.key({ modkey,           }, 'j',
        function ()
            AwesomeApi.awful.client.focus.byidx( 1)
        end,
        {description = 'focus next by index', group = 'client'}
    ),
    AwesomeApi.awful.key({ modkey,           }, 'k',
        function ()
            AwesomeApi.awful.client.focus.byidx(-1)
        end,
        {description = 'focus previous by index', group = 'client'}
    ),
    AwesomeApi.awful.key({ modkey, 'Shift'   }, 'j', function () AwesomeApi.awful.client.swap.byidx(  1)    end,
      {description = 'swap with next client by index', group = 'client'}),
    AwesomeApi.awful.key({ modkey, 'Shift'   }, 'k', function () AwesomeApi.awful.client.swap.byidx( -1)    end,
      {description = 'swap with previous client by index', group = 'client'}),
    AwesomeApi.awful.key({ modkey,           }, 'u', AwesomeApi.awful.client.urgent.jumpto,
      {description = 'jump to urgent client', group = 'client'}),
    AwesomeApi.awful.key({ modkey,           }, 'Tab',
      function ()
        AwesomeApi.awful.client.focus.history.previous()
        if client.focus then
          client.focus:raise()
        end
      end,
      {description = 'go back', group = 'client'}),

    -- screen
    AwesomeApi.awful.key({ modkey, 'Control' }, 'j', function () AwesomeApi.awful.focus_relative( 1) end,
              {description = 'focus the next screen', group = 'screen'}),
    AwesomeApi.awful.key({ modkey, 'Control' }, 'k', function () AwesomeApi.awful.focus_relative(-1) end,
              {description = 'focus the previous screen', group = 'screen'}),

    -- launcher
    AwesomeApi.awful.key({ modkey,           }, 'Return', function () AwesomeApi.awful.spawn(terminal) end,
              {description = 'open a terminal', group = 'launcher'}),
    AwesomeApi.awful.key({ modkey,           }, 'a', function () AwesomeApi.awful.spawn(gui_editor) end,
              {description = 'open the editor', group = 'launcher'}),
    AwesomeApi.awful.key({ modkey,           }, 'q', function () AwesomeApi.awful.spawn(browser) end,
              {description = 'open the broswer', group = 'launcher'}),

    -- layout
    AwesomeApi.awful.key({ modkey,           }, 'l',     function () AwesomeApi.awful.tag.incmwfact( 0.05)          end,
              {description = 'increase master width factor', group = 'layout'}),
    AwesomeApi.awful.key({ modkey,           }, 'h',     function () AwesomeApi.awful.tag.incmwfact(-0.05)          end,
              {description = 'decrease master width factor', group = 'layout'}),
    AwesomeApi.awful.key({ modkey, 'Shift'   }, 'h',     function () AwesomeApi.awful.tag.incnmaster( 1, nil, true) end,
              {description = 'increase the number of master clients', group = 'layout'}),
    AwesomeApi.awful.key({ modkey, 'Shift'   }, 'l',     function () AwesomeApi.awful.tag.incnmaster(-1, nil, true) end,
              {description = 'decrease the number of master clients', group = 'layout'}),
    AwesomeApi.awful.key({ modkey, 'Control' }, 'h',     function () AwesomeApi.awful.tag.incncol( 1, nil, true)    end,
              {description = 'increase the number of columns', group = 'layout'}),
    AwesomeApi.awful.key({ modkey, 'Control' }, 'l',     function () AwesomeApi.awful.tag.incncol(-1, nil, true)    end,
              {description = 'decrease the number of columns', group = 'layout'}),
    AwesomeApi.awful.key({ modkey,           }, 'space', function () AwesomeApi.awful.layout.inc( 1)                end,
              {description = 'select next', group = 'layout'}),
    AwesomeApi.awful.key({ modkey, 'Shift'   }, 'space', function () AwesomeApi.awful.layout.inc(-1)                end,
              {description = 'select previous', group = 'layout'}),

    AwesomeApi.awful.key({ modkey, 'Control' }, 'n',
              function ()
                  local c = AwesomeApi.awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        'request::activate', 'key.unminimize', {raise = true}
                    )
                  end
              end,
              {description = 'restore minimized', group = 'client'}),

    -- Prompt
    AwesomeApi.awful.key({ modkey },            'r',     function () AwesomeApi.awful.focused().mypromptbox:run() end,
              {description = 'run prompt', group = 'launcher'}),

    AwesomeApi.awful.key({ modkey }, 'x',
              function ()
                  AwesomeApi.awful.prompt.run {
                    prompt       = 'Run Lua code: ',
                    textbox      = AwesomeApi.awful.focused().mypromptbox.widget,
                    exe_callback = AwesomeApi.awful.util.eval,
                    history_path = AwesomeApi.awful.util.get_cache_dir() .. '/history_eval'
                  }
              end,
              {description = 'lua execute prompt', group = 'awesome'}),
    -- Menubar
    AwesomeApi.awful.key({ modkey }, 'p', function() menubar.show() end,
              {description = 'show the menubar', group = 'launcher'})
)

local clientkeys = AwesomeApi.gears.table.join(
    AwesomeApi.awful.key({ modkey,           }, 'f',
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = 'toggle fullscreen', group = 'client'}),
    AwesomeApi.awful.key({ modkey, 'Shift'   }, 'c',      function (c) c:kill()                         end,
              {description = 'close', group = 'client'}),
    AwesomeApi.awful.key({ modkey, 'Control' }, 'space',  AwesomeApi.awful.client.floating.toggle                     ,
              {description = 'toggle floating', group = 'client'}),
    AwesomeApi.awful.key({ modkey, 'Control' }, 'Return', function (c) c:swap(AwesomeApi.awful.client.getmaster()) end,
              {description = 'move to master', group = 'client'}),
    AwesomeApi.awful.key({ modkey,           }, 'o',      function (c) c:move_to_screen()               end,
              {description = 'move to screen', group = 'client'}),
    AwesomeApi.awful.key({ modkey,           }, 't',      function (c) c.ontop = not c.ontop            end,
              {description = 'toggle keep on top', group = 'client'}),
    AwesomeApi.awful.key({ modkey,           }, 'n',
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = 'minimize', group = 'client'}),
    AwesomeApi.awful.key({ modkey,           }, 'm',
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = '(un)maximize', group = 'client'}),
    AwesomeApi.awful.key({ modkey, 'Control' }, 'm',
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = '(un)maximize vertically', group = 'client'}),
    AwesomeApi.awful.key({ modkey, 'Shift'   }, 'm',
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
    globalkeys = AwesomeApi.gears.table.join(globalkeys,
        -- View tag only.
        AwesomeApi.awful.key({ modkey }, '#' .. i + 9,
                  function ()
                        local screen = AwesomeApi.awful.focused()
                        local tag = AwesomeApi.screens.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = 'view tag #'..i, group = 'tag'}),
        -- Toggle tag display.
        AwesomeApi.awful.key({ modkey, 'Control' }, '#' .. i + 9,
                  function ()
                      local screen = AwesomeApi.awful.focused()
                      local tag = AwesomeApi.screens.tags[i]
                      if tag then
                         AwesomeApi.awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = 'toggle tag #' .. i, group = 'tag'}),
        -- Move client to tag.
        AwesomeApi.awful.key({ modkey, 'Shift' }, '#' .. i + 9,
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
        AwesomeApi.awful.key({ modkey, 'Control', 'Shift' }, '#' .. i + 9,
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

local clientbuttons = AwesomeApi.gears.table.join(
    AwesomeApi.awful.button({ }, 1, function (c)
        c:emit_signal('request::activate', 'mouse_click', {raise = true})
    end),
    AwesomeApi.awful.button({ modkey }, 1, function (c)
        c:emit_signal('request::activate', 'mouse_click', {raise = true})
        AwesomeApi.awful.mouse.client.move(c)
    end),
    AwesomeApi.awful.button({ modkey }, 3, function (c)
        c:emit_signal('request::activate', 'mouse_click', {raise = true})
        AwesomeApi.awful.mouse.client.resize(c)
    end)
)

-- Set keys
AwesomeApi.rootWindow.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the 'manage' signal).
AwesomeApi.awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = AwesomeApi.beautiful.border_width,
                     border_color = AwesomeApi.beautiful.border_normal,
                     focus = AwesomeApi.awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = AwesomeApi.awful.screen.preferred,
                     placement = AwesomeApi.awful.placement.no_overlap+AwesomeApi.awful.placement.no_offscreen,
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
AwesomeApi.clients.connect_signal('manage', function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not AwesomeApi.startup then AwesomeApi.awful.client.setslave(c) end


    if AwesomeApi.startup
        and not c.size_hints.user_position
        and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        AwesomeApi.awful.placement.no_offscreen(c)
    end

    c.shape = AwesomeApi.gears.shape.rounded_rect
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
AwesomeApi.clients.connect_signal('request::titlebars', function(c)
    -- buttons for the titlebar
    local buttons = AwesomeApi.gears.table.join(
        AwesomeApi.awful.button({ }, 1, function()
            c:emit_signal('request::activate', 'titlebar', {raise = true})
            AwesomeApi.awful.mouse.client.move(c)
        end),
        AwesomeApi.awful.button({ }, 3, function()
            c:emit_signal('request::activate', 'titlebar', {raise = true})
            AwesomeApi.awful.mouse.client.resize(c)
        end)
    )

    AwesomeApi.awful.titlebar(c) : setup {
        { -- Left
            AwesomeApi.awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = AwesomeApi.wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = 'center',
                widget = AwesomeApi.awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = AwesomeApi.wibox.layout.flex.horizontal
        },
        { -- Right
            AwesomeApi.awful.titlebar.widget.floatingbutton (c),
            AwesomeApi.awful.titlebar.widget.maximizedbutton(c),
            AwesomeApi.awful.titlebar.widget.stickybutton   (c),
            AwesomeApi.awful.titlebar.widget.ontopbutton    (c),
            AwesomeApi.awful.titlebar.widget.closebutton    (c),
            layout = AwesomeApi.wibox.layout.fixed.horizontal()
        },
        layout = AwesomeApi.wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
AwesomeApi.clients.connect_signal('mouse::enter', function(c)
    c:emit_signal('request::activate', 'mouse_enter', {raise = true})
end)
-- }}}
