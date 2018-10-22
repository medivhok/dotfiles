

local gruvbox = {

  -- global colors
  dark0_hard  = '#1d2021',  -- 29-32-33
  dark0       = '#282828',  -- 40-40-40
  dark0_soft  = '#32302f',  -- 50-48-47
  dark1       = '#3c3836',  -- 60-56-54
  dark2       = '#504945',  -- 80-73-69
  dark3       = '#665c54',  -- 102-92-84
  dark4       = '#7c6f64',  -- 124-111-100
  dark4_256   = '#7c6f64',  -- 124-111-100

  gray_245    = '#928374',  -- 146-131-116
  gray_244    = '#928374',  -- 146-131-116

  light0_hard = '#f9f5d7',  -- 249-245-215
  light0      = '#fbf1c7',  -- 253-244-193
  light0_soft = '#f2e5bc',  -- 242-229-188
  light1      = '#ebdbb2',  -- 235-219-178
  light2      = '#d5c4a1',  -- 213-196-161
  light3      = '#bdae93',  -- 189-174-147
  light4      = '#a89984',  -- 168-153-132
  light4_256  = '#a89984',  -- 168-153-132

  bright_red     = '#fb4934',  -- 251-73-52
  bright_green   = '#b8bb26',  -- 184-187-38
  bright_yellow  = '#fabd2f',  -- 250-189-47
  bright_blue    = '#83a598',  -- 131-165-152
  bright_purple  = '#d3869b',  -- 211-134-155
  bright_aqua    = '#8ec07c',  -- 142-192-124
  bright_orange  = '#fe8019',  -- 254-128-25

  neutral_red    = '#cc241d',  -- 204-36-29
  neutral_green  = '#98971a',  -- 152-151-26
  neutral_yellow = '#d79921',  -- 215-153-33
  neutral_blue   = '#458588',  -- 69-133-136
  neutral_purple = '#b16286',  -- 177-98-134
  neutral_aqua   = '#689d6a',  -- 104-157-106
  neutral_orange = '#d65d0e',  -- 214-93-14

  faded_red      = '#9d0006',  -- 157-0-6
  faded_green    = '#79740e',  -- 121-116-14
  faded_yellow   = '#b57614',  -- 181-118-20
  faded_blue     = '#076678',  -- 7-102-120
  faded_purple   = '#8f3f71',  -- 143-63-113
  faded_aqua     = '#427b58',  -- 66-123-88
  faded_orange   = '#af3a03',  -- 175-58-3
}


gruvbox.dark_theme = {
  bg0 = gruvbox.dark0,
  bg1 = gruvbox.dark1,
  bg2 = gruvbox.dark2,
  bg3 = gruvbox.dark3,
  bg4 = gruvbox.dark4,

  gray = gruvbox.gray_245,

  fg0 = gruvbox.light0,
  fg1 = gruvbox.light1,
  fg2 = gruvbox.light2,
  fg3 = gruvbox.light3,
  fg4 = gruvbox.light4,

  fg4_256 = gruvbox.light4_256,

  red    = gruvbox.bright_red,
  green  = gruvbox.bright_green,
  yellow = gruvbox.bright_yellow,
  blue   = gruvbox.bright_blue,
  purple = gruvbox.bright_purple,
  aqua   = gruvbox.bright_aqua,
  orange = gruvbox.bright_orange,
}

gruvbox.dark_theme_soft = setmetatable(
  { bg0 = gruvbox.dark0_soft },
  { __index = gruvbox.dark_theme }
)

gruvbox.dark_theme_hard = setmetatable(
  { bg0 = gruvbox.dark0_hard },
  { __index = gruvbox.dark_theme }
)

gruvbox.light_theme = {
  bg0 = gruvbox.light0,
  bg1 = gruvbox.light1,
  bg2 = gruvbox.light2,
  bg3 = gruvbox.light3,
  bg4 = gruvbox.light4,

  gray = gruvbox.gray_244,

  fg0 = gruvbox.dark0,
  fg1 = gruvbox.dark1,
  fg2 = gruvbox.dark2,
  fg3 = gruvbox.dark3,
  fg4 = gruvbox.dark4,

  fg4_256 = gruvbox.dark4_256,

  red    = gruvbox.faded_red,
  green  = gruvbox.faded_green,
  yellow = gruvbox.faded_yellow,
  blue   = gruvbox.faded_blue,
  purple = gruvbox.faded_purple,
  aqua   = gruvbox.faded_aqua,
  orange = gruvbox.faded_orange,
}

gruvbox.light_theme_soft = setmetatable(
  { bg0 = gruvbox.light0_soft },
  { __index = gruvbox.light_theme }
)

gruvbox.light_theme_hard = setmetatable(
  { bg0 = gruvbox.light0_hard },
  { __index = gruvbox.light_theme }
)


return gruvbox
