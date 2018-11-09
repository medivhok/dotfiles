return {
    browser  = 'chromium',
    editor   = 'emacs',
    modkey   = 'Mod4',
    terminal = 'termite',

    gshape = {
      roundedRectRadius = 6,
    },

    theme = {
       -- beautiful configs
       name = 'default',
       screens = {
          [1] = 2,
          [2] = 1,
          [3] = 3,
       },
    },

    ui = {
       layouts = {
          [1] = 'tile.bottom',
          [2] = 'tile'
       }
    },
}
