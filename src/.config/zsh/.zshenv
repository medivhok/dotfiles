export XDG_DESKTOP_DIR=${XDG_DESKTOP_DIR:-$HOME/Desktop}
export XDG_DOCUMENTS_DIR=${XDG_DOCUMENTS_DIR:-$HOME/Documents}
export XDG_DOWNLOAD_DIR=${XDG_DOWNLOAD_DIR:-$HOME/Downloads}
export XDG_MUSIC_DIR=${XDG_MUSIC_DIR:-$HOME/Music}
export XDG_PICTURES_DIR=${XDG_PICTURES_DIR:-$HOME/Pictures}
export XDG_PUBLICSHARE_DIR=${XDG_PUBLICSHARE_DIR:-$HOME/Public}
export XDG_TEMPLATES_DIR=${XDG_TEMPLATES_DIR:-$HOME/Templates}
export XDG_VIDEOS_DIR=${XDG_VIDEOS_DIR:-$HOME/Videos}

# cabal, npm and yarn executables
export PATH=$HOME/.dotnet/tools:$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.npm/bin:$HOME/.local/bin:$PATH

export NVM_DIR="$XDG_CONFIG_HOME/nvm"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.local/share/oh-my-zsh"

export EDITOR=vim

export QT_QPA_PLATFORMTHEME=qt5ct

export LEDGER_FILE="~/Documents/Budget/ledger/main.ledger"