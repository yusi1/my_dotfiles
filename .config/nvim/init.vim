" Plugin Directory
call plug#begin('~/.local/share/nvim/plugged')

" This is where you specify plugins
" - Make sure to use single quotes

"Plug 'itchyny/landscape.vim'
Plug 'scrooloose/nerdtree'
Plug 'itchyny/lightline.vim'
"Plug 'romgrk/doom-one.vim'
Plug 'sainnhe/edge'
"Plug 'ghifarit53/tokyonight-vim'
"Plug 'morhetz/gruvbox'
"Plug 'shinchu/lightline-gruvbox.vim'
"Plug 'joshdick/onedark.vim'
"Plug 'overcache/NeoSolarized'

" Initialize plugins
call plug#end()

" Set theme
set termguicolors

" -- Edge config --
syntax on
colorscheme edge
"let g:edge_style = 'edge'
let g:edge_enable_italic = 0
let g:edge_disable_italic_comment = 0
let g:lightline = {}
        let g:lightline.colorscheme = 'edge'

" -- Distrotube's config's colourscheme (doom one & dracula mixed)
"syntax on
"colorscheme doom-one
"let g:lightline = {
    "\ 'colorscheme': 'doom-one',
    "\ }

" -- Landscape Theme Config --
"syntax on
"colorscheme landscape
"let g:lightline = {
    "\ 'colorscheme': 'landscape',
    "\ }

"-----------------------------

" -- Solarized Theme Configuration --

"set termguicolors
"colorscheme NeoSolarized
"set background=dark
"let g:lightline = {
            "\ 'colorscheme': 'solarized',
            "\ }

" If you wish to enable/disable NeoSolarized from displaying bold, underlined
" or italicized" typefaces, simply assign 1 or 0 to the appropriate variable.
" Default values:
"let g:neosolarized_bold = 1
"let g:neosolarized_underline = 1
"let g:neosolarized_italic = 1

"------------------------------------


" -- Onedark Theme Configuration --
"syntax on
"colorscheme onedark
"let g:lightline = {
    "\ 'colorscheme': 'onedark',
    "\ }
"----------------------------------

" -- Gruvbox Theme Configuration --
"autocmd vimenter * ++nested colorscheme gruvbox
"let g:lightline = {}
"let g:lightline.colorscheme = 'gruvbox'
" ---------------------------------

" -- Tokyonight Theme Configuration --
"let g:tokyonight_style = 'night' " available: night, storm
"let g:tokyonight_enable_italic = 1
"colorscheme tokyonight
"let g:lightline = {'colorscheme' : 'tokyonight'}
" ------------------------------------


" -- Configuration Options --

set nu
set rnu

set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=4

"hi Normal guibg=NONE ctermbg=NONE
