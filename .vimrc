nnoremap <Leader>d :make doc<CR>
nnoremap <Leader>m :make build<CR>

" recognize test error locations
let &errorformat="### Failure in %f:%l: %m," . &errorformat

" put all paragraphs on single lines
command! CompressParagraphs %s/\n\zs\%(\s*\n\)\+\%(\s*\S.*\%(\n\s*\S.*\)*\)\+/\=substitute(substitute(submatch(0),'\_s\+',' ','g'),'^\s*\|\s*$','','g')/ | nohl
