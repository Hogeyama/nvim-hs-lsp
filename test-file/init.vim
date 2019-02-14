syntax on
set noswapfile
let g:NvimHsLsp_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'ocaml': ['ocaml-language-server', '--stdio'],
    \ 'haskell': ['stack', 'exec', '--', 'hie', '--lsp', '-d', '-l', '/tmp/hie.log'],
    \ }

