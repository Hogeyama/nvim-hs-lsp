# nvim-hs-lsp

[Language Server Protcol](https://microsoft.github.io/language-server-protocol/specification) implementation for neovim using [nvim-hs](https://github.com/neovimhaskell/nvim-hs).

## Screen shot(s)

![](https://github.com/Hogeyama/nvim-hs-lsp/blob/media/screenshot/Sample.gif)


## Implemented

+ Realtime diagnostics/lint messages
+ Jump to definition
+ Hover (get identifier infomation)
+ Apply refact of [hie](https://github.com/haskell/haskell-ide-engine) (halfway).

## TODO

- [ ] complete applyRefact for hie

      + partialiy implemented

- [ ] implement completion

- [ ] implement rename

- [ ] appropriately classify input with method `$/...` into Request/Notification.
      (Currently, all messages are parsed as Notification.)

