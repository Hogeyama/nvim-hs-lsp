# nvim-hs-lsp

[Language Server Protcol](https://microsoft.github.io/language-server-protocol/specification) implementation for neovim using [nvim-hs](https://github.com/neovimhaskell/nvim-hs).

[![CircleCI](https://circleci.com/gh/Hogeyama/nvim-hs-lsp/tree/master.svg?style=svg)](https://circleci.com/gh/Hogeyama/nvim-hs-lsp/tree/master)

## Installation

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. Install [nvim-hs.vim](https://github.com/neovimhaskell/nvim-hs.vim) and this plugin

    If you are using vim-plug:

    ```vim
    Plug 'neovimhaskell/nvim-hs.vim'
    Plug 'Hogeyama/nvim-hs-lsp'
    ```

    Note: It takes quite a long time to install.

## Screen shots

### Code action

<img src="./screenshot/CodeAction.gif" width="600">

### Jump to definition

<img src="./screenshot/Definition.gif" width="600">

### Completion

+ `completefunc`
+ [`deoplete`](https://github.com/Shougo/deoplete.nvim) source

<img src="./screenshot/Completion.gif" width="600">

