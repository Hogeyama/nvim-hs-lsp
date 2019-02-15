
let s:project_dir = expand('<sfile>:p:h:h')
let s:project_name = 'nvim-hs-lsp'

function! s:start_nvim_hs_lsp(...) abort
endfunction
call s:start_nvim_hs_lsp()

let s:starter = get(g:, 'nvimhsPluginStarter', nvimhs#stack#pluginstarter())

function! s:executeAsync(directory, cmd)
  let l:out = []
  let l:err = []
  let l:FonExit = funcref('s:onExit2', [a:directory, a:cmd, l:out, l:err])
  return jobstart(a:cmd,
        \ { 'on_stdout': {jobId, data, event -> extend(l:out, data)}
        \ , 'stdout_buffered': 1
        \ , 'on_stderr': {jobId, data, event -> extend(l:err, data)}
        \ , 'stderr_buffered': 1
        \ , 'cwd': a:directory
        \ , 'on_exit': l:FonExit
        \ })
endfunction

function! s:onExit2(directory, cmd, out, err, jobId, exitCode, event)
  let l:out = join(filter(a:out, 'v:val isnot ""'))
  let l:err = join(filter(a:err, 'v:val isnot ""'))
  if a:exitCode != 0
    echohl Error
    echom 'Failed to execute (cwd: ' . a:directory . '): ' . a:cmd
    echom ' stderr: ' . l:err
    echohl None
  else
    try
      call nvimhs#start(s:project_dir, s:project_name, [])
    catch /^Vim(call):E117/
      echoerr "nvim-hs.vim is not installed"
    endtry
  endif
endfunction

call s:executeAsync(s:project_dir, s:starter.buildCommand(s:project_name))
