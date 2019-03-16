syntax on
set noswapfile
let g:NvimHsLsp_languageConfig = {
      \   '_': {
      \     'autoloadQuickfix': v:true,
      \   },
      \   'haskell': {
      \     'serverCommand':
      \       ['hie-wrapper', '--lsp', '-d', '-l', '/tmp/test-hie.log'],
      \     'formattingOptions': {
      \       'tabSize': 2,
      \       'insertSpaces': v:true,
      \     },
      \   },
      \ }
