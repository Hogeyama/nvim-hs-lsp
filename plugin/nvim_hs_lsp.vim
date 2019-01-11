try
  call nvimhs#start(expand('<sfile>:p:h:h'), 'nvim-hs-lsp', [])
catch /^Vim(call):E117/
  echoerr "nvim-hs.vim is not installed"
endtry

