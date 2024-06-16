" The java.vim indent file does not properly indent Java annotations. This
" function will replace the indent existing indent function and dlegate
" to it. However, the result will be modified for lines with annotations
" to subtract one so it indents properly.
"
" Source:
" http://stackoverflow.com/questions/200932/how-do-i-make-vim-indent-java-annotations-correctly
function! GetJavaIndent_improved()
  let lnum = prevnonblank(v:lnum - 1)
  let line = getline(lnum)
  if line =~ '^\s*@.*$'
    let theIndent = indent(lnum)
  endif

  return theIndent
endfunction

setlocal indentexpr=GetJavaIndent_improved()
