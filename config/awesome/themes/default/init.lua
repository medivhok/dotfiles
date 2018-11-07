---------------------------------------------------------------------------------------------------
--- The default theme.
-- @module themes.default
-- @author Jean Gregory Verret <gregory.verret@gmail.com>
-- @copyright 2018 Jean Gregory Verret
-- @license MIT
---------------------------------------------------------------------------------------------------
local gruvbox = require('gruvbox')

return function(awesomeApi, configs)
  local gears = awesomeApi.gears

  local themes_path = gears.filesystem.get_themes_dir()
  local dpi = awesomeApi.dpi
  local wallpapers_dir = gears.filesystem.get_configuration_dir() .. '/wallpapers/'

  local colors = gruvbox.dark_theme

  -- colors
  local fg_light  = '#ffffff'
  local fg_medium = '#ffffffb2'
  local fg_dark   = '#ffffffba'
  local bg_light  = '#424242'
  local bg_medium = '#303030'
  local bg_dark   = '#212121'


  -- wallpapers
  local wallpapers = {}
  wallpapers[1] = wallpapers_dir .. 'screen_01.jpg'
  wallpapers[2] = wallpapers_dir .. 'screen_02.jpg'
  wallpapers[3] = wallpapers_dir .. 'screen_03.jpg'


  local theme = {}
  theme.wallpaper = function(s)
    local wallpaper = wallpapers[s.index]

    return wallpaper or wallpapers[#wallpapers]
  end


  -- {{{ Styles
  theme.font      = "Roboto 10"

  -- {{{ Colors
  theme.fg_normal  = colors.fg2
  theme.fg_focus   = colors.fg1
  theme.fg_urgent  = colors.fg0
  theme.bg_normal  = colors.bg0
  theme.bg_focus   = colors.bg1
  theme.bg_urgent  = colors.bg2
  theme.bg_systray = theme.bg_normal
-- }}}

  -- {{{ Borders
  theme.useless_gap   = dpi(4)
  theme.border_width  = dpi(4)
  theme.border_normal = theme.bg_normal
  -- }}}

  -- {{{ Titlebars
  -- theme.titlebar_bg_focus  = bg_light
  -- theme.titlebar_bg_normal = bg_medium
  -- }}}

  -- There are other variable sets
  -- overriding the default one when
  -- defined, the sets are:
  -- [taglist|tasklist]_[bg|fg]_[focus|urgent|occupied|empty|volatile]
  -- titlebar_[normal|focus]
  -- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
  -- Example:
  --theme.taglist_bg_focus = "#CC9393"
  -- }}}

  -- {{{ Widgets
  -- You can add as many variables as
  -- you wish and access them by using
  -- beautiful.variable in your rc.lua
  --theme.fg_widget        = "#AECF96"
  --theme.fg_center_widget = "#88A175"
  --theme.fg_end_widget    = "#FF5656"
  --theme.bg_widget        = "#494B4F"
  --theme.border_widget    = "#3F3F3F"
  -- }}}

  -- {{{ Mouse finder
  theme.mouse_finder_color = "#CC9393"
  -- mouse_finder_[timeout|animate_timeout|radius|factor]
  -- }}}

  -- {{{ Menu
  -- Variables set for theming the menu:
  -- menu_[bg|fg]_[normal|focus]
  -- menu_[border_color|border_width]
  theme.menu_height = dpi(20)
  theme.menu_width  = dpi(200)
  -- }}}

  -- {{{ Icons
  -- {{{ Taglist
  theme.taglist_squares_sel   = themes_path .. "zenburn/taglist/squarefz.png"
  theme.taglist_squares_unsel = themes_path .. "zenburn/taglist/squarez.png"
  --theme.taglist_squares_resize = "false"
  -- }}}

  -- {{{ Misc
  theme.awesome_icon           = themes_path .. "zenburn/awesome-icon.png"
  theme.menu_submenu_icon      = themes_path .. "default/submenu.png"
  -- }}}

  -- {{{ Layout
  theme.layout_tile       = themes_path .. "zenburn/layouts/tile.png"
  theme.layout_tileleft   = themes_path .. "zenburn/layouts/tileleft.png"
  theme.layout_tilebottom = themes_path .. "zenburn/layouts/tilebottom.png"
  theme.layout_tiletop    = themes_path .. "zenburn/layouts/tiletop.png"
  theme.layout_fairv      = themes_path .. "zenburn/layouts/fairv.png"
  theme.layout_fairh      = themes_path .. "zenburn/layouts/fairh.png"
  theme.layout_spiral     = themes_path .. "zenburn/layouts/spiral.png"
  theme.layout_dwindle    = themes_path .. "zenburn/layouts/dwindle.png"
  theme.layout_max        = themes_path .. "zenburn/layouts/max.png"
  theme.layout_fullscreen = themes_path .. "zenburn/layouts/fullscreen.png"
  theme.layout_magnifier  = themes_path .. "zenburn/layouts/magnifier.png"
  theme.layout_floating   = themes_path .. "zenburn/layouts/floating.png"
  theme.layout_cornernw   = themes_path .. "zenburn/layouts/cornernw.png"
  theme.layout_cornerne   = themes_path .. "zenburn/layouts/cornerne.png"
  theme.layout_cornersw   = themes_path .. "zenburn/layouts/cornersw.png"
  theme.layout_cornerse   = themes_path .. "zenburn/layouts/cornerse.png"
  -- }}}

  -- {{{ Titlebar
  theme.titlebar_close_button_focus  = themes_path .. "zenburn/titlebar/close_focus.png"
  theme.titlebar_close_button_normal = themes_path .. "zenburn/titlebar/close_normal.png"

  theme.titlebar_minimize_button_normal = themes_path .. "default/titlebar/minimize_normal.png"
  theme.titlebar_minimize_button_focus  = themes_path .. "default/titlebar/minimize_focus.png"

  theme.titlebar_ontop_button_focus_active  = themes_path .. "zenburn/titlebar/ontop_focus_active.png"
  theme.titlebar_ontop_button_normal_active = themes_path .. "zenburn/titlebar/ontop_normal_active.png"
  theme.titlebar_ontop_button_focus_inactive  = themes_path .. "zenburn/titlebar/ontop_focus_inactive.png"
  theme.titlebar_ontop_button_normal_inactive = themes_path .. "zenburn/titlebar/ontop_normal_inactive.png"

  theme.titlebar_sticky_button_focus_active  = themes_path .. "zenburn/titlebar/sticky_focus_active.png"
  theme.titlebar_sticky_button_normal_active = themes_path .. "zenburn/titlebar/sticky_normal_active.png"
  theme.titlebar_sticky_button_focus_inactive  = themes_path .. "zenburn/titlebar/sticky_focus_inactive.png"
  theme.titlebar_sticky_button_normal_inactive = themes_path .. "zenburn/titlebar/sticky_normal_inactive.png"

  theme.titlebar_floating_button_focus_active  = themes_path .. "zenburn/titlebar/floating_focus_active.png"
  theme.titlebar_floating_button_normal_active = themes_path .. "zenburn/titlebar/floating_normal_active.png"
  theme.titlebar_floating_button_focus_inactive  = themes_path .. "zenburn/titlebar/floating_focus_inactive.png"
  theme.titlebar_floating_button_normal_inactive = themes_path .. "zenburn/titlebar/floating_normal_inactive.png"

  theme.titlebar_maximized_button_focus_active  = themes_path .. "zenburn/titlebar/maximized_focus_active.png"
  theme.titlebar_maximized_button_normal_active = themes_path .. "zenburn/titlebar/maximized_normal_active.png"
  theme.titlebar_maximized_button_focus_inactive  = themes_path .. "zenburn/titlebar/maximized_focus_inactive.png"
  theme.titlebar_maximized_button_normal_inactive = themes_path .. "zenburn/titlebar/maximized_normal_inactive.png"

  theme.wibar_height = 34

  return theme
end
