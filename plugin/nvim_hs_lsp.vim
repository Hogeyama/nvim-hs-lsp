
let s:project_dir = expand('<sfile>:p:h:h')
let s:project_name = 'nvim-hs-lsp'
let s:starter = get(g:, 'nvimhsPluginStarter', nvimhs#stack#pluginstarter())
let s:buildCommand = s:starter.buildCommand(s:project_name)

" This will be overridden when nvim-hs-lsp is loaded
function! NvimHsLspComplete(findStart, base) abort
  echomsg "nvim-hs-lsp is not loaded"
  if a:findStart
    return -3
  else
    return {}
  endif
endfunction
function! NvimHsLspAsyncComplete(...) abort
  let g:NvimHsLspCompleteResult = {}
endfunction

function! s:buildAsyncThenStart()
  let l:out = []
  let l:err = []
  let l:FonExit = funcref('s:afterBuild', [l:out, l:err])
  return jobstart(s:buildCommand,
        \ { 'on_stdout': {jobId, data, event -> extend(l:out, data)}
        \ , 'stdout_buffered': 1
        \ , 'on_stderr': {jobId, data, event -> extend(l:err, data)}
        \ , 'stderr_buffered': 1
        \ , 'cwd': s:project_dir
        \ , 'on_exit': l:FonExit
        \ })
endfunction

function! s:afterBuild(out, err, jobId, exitCode, event)
  let l:out = join(filter(a:out, 'v:val isnot ""'))
  let l:err = join(filter(a:err, 'v:val isnot ""'))
  if a:exitCode != 0
    echohl Error
    echom 'Failed to execute (cwd: ' . s:project_dir . '): ' . s:buildCommand
    echom ' stderr: ' . l:err
    echohl None
  else
    " echo "nvim-hs-lsp build complete"
    try
      call nvimhs#start(s:project_dir, s:project_name, [])
    catch /^Vim(call):E117/
      echoerr "nvim-hs.vim is not installed"
    endtry
  endif
endfunction

call s:buildAsyncThenStart()
