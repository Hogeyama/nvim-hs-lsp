syntax on
set noswapfile
let g:NvimHsLsp_languageConfig = {
      \   '_': {
      \     'settingsPath': expand("$HOME/.config/nvim/settings.json"),
      \     'autoloadQuickfix': v:true,
      \   },
      \   'haskell': {
      \     'serverCommand':
      \       ['hie-wrapper', '--lsp', '-d', '-l', '/tmp/LanguageServer.log'],
      \     'formattingOptions': {
      \       'tabSize': 2,
      \       'insertSpaces': v:true,
      \     },
      \   },
      \   'rust': {
      \     'serverCommand':
      \       ['rustup', 'run', 'stable', 'rls'],
      \     'formattingOptions': {
      \       'tabSize': 4,
      \       'insertSpaces': v:true,
      \     },
      \   },
      \   'ocaml': {
      \     'serverCommand':
      \       ['ocaml-language-server', '--stdio'],
      \     'formattingOptions': {
      \       'tabSize': 2,
      \       'insertSpaces': v:true,
      \     },
      \   },
      \   'elm': {
      \     'serverCommand':
      \       [ 'elm-language-server', '-l', '/tmp/LanguageServer.log'],
      \     'formattingOptions': {
      \       'tabSize': 2,
      \       'insertSpaces': v:true,
      \     },
      \   },
      \   'python': {
      \     'serverCommand':
      \       ['pyls', '-v', '--log-file', '/tmp/pyls.log'],
      \     'formattingOptions': {
      \       'tabSize': 4,
      \       'insertSpaces': v:true,
      \     },
      \   },
      \ }
