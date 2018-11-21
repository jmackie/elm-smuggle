nmap <c-b> :make<cr>

autocmd Filetype haskell setlocal makeprg=stack\ install
"autocmd Filetype haskell setlocal makeprg=cabal\ new-build\ &&\ rm\ -f\ .ghc.environment.*
