" Vim global plugin for setting automatically the foldcolumn option based on
" the window width.
"
" Maintainer: Thierry Rascle <thierr26@free.fr>
"
" License: Unlicense. Please refer to the LICENSE file.

" Compute the root of the base name of the current script (i.e. with the
" extension removed), in lower case.
let s:script = tolower(fnamemodify(expand('<sfile>:p'), ':t:r'))

" Check that the resulting name is made of lower case a to z letters and digits
" (but no digit at first position).
if s:script !~# '^[a-z][a-z0-9]*$'
    throw '<sfile>:p' . " is not a suitable file name for the plugin"
endif

if exists("g:loaded_" . s:script)
    " The user has already set g:loaded_wibafoco to disable loading this plugin
    " or the plugin has already been loaded.

    " Stop sourcing the script.
    finish
endif
let g:loaded_{s:script} = 1

" Store the value of cpoptions.
let s:save_cpo = &cpo

" Compute the common prefix for all the plugin related variables, including a
" trailing underscore.
let s:prefix = s:script . "_"

" Compute the plugin name (with the first letter capitalized).
let s:plugin = substitute(s:script, "^.", '\=toupper(submatch(0))', "")

" Set cpoptions to its Vim default.
set cpo&vim

" -----------------------------------------------------------------------------

" Checks that the argument is a valid identifier for a plugin related
" parameter.
"
" Arguments:
"
" #1 - s
" Anything.
"
" Return value:
" Non-zero if the argument is a valid identifier for a plugin related
" parameter, zero otherwise.
function s:IsParamIdent(s)
    return type(a:s) == type("") && a:s =~# '^[a-z][a-z0-9]*\(_[a-z0-9]\+\)*$'
endfunction

" -----------------------------------------------------------------------------

" Checks that the argument is a valid identifier for a plugin related function.
"
" Arguments:
"
" #1 - s
" Anything.
"
" Return value:
" Non-zero if the argument is a valid identifier for a plugin related function,
" zero otherwise.
function s:IsFuncIdent(s)
    return type(a:s) == type("") && a:s =~# '^[A-Z][A-Za-z0-9]*$'
endfunction

" -----------------------------------------------------------------------------

" Checks that the argument is a non-empty string.
"
" Arguments:
"
" #1 - s
" Anything.
"
" Return value:
" Non-zero if the argument is a non-empty string, zero otherwise.
function s:IsNonEmptyString(s)
    return type(a:s) == type("") && strlen(a:s) > 0
endfunction

" -----------------------------------------------------------------------------

" Checks the existence of a global variable. The name of the variable is "g:"
" followed by s:prefix ("wibafoco_") and followed by the argument.
"
" Arguments:
"
" #1 - ident
" Part of the variable identifier.
"
" Return value:
" Non-zero if the global variable exists, zero otherwise.
function s:ExistsAsGlobal(ident)

    " Check the argument.
    if !s:IsParamIdent(a:ident)
        throw "Invalid identifier"
    endif

    return exists("g:" . s:prefix . a:ident)
endfunction

" -----------------------------------------------------------------------------

" Returns the value of a global variable. Designed to be only used after a call
" to the function s:ExistsAsGlobal (with exactly the same argument) that has
" returned a non-zero value. Use in any other condition is inappropriate.
"
" Arguments:
"
" #1 - ident
" Last word in the variable identifier.
"
" Return value:
" Value of the global variable.
function s:GetGlobal(ident, ...)
    return g:{s:prefix}{a:ident}
endfunction

" -----------------------------------------------------------------------------

" Returns the value of a plugin related parameter.
"
" The statement: let g:x = s:Get("foo", "bar", function("len"))
" results in:
" - an exception being thrown if g:wibafoco_foo exists and the len function
"   applied to it returns zero,
" - or g:x being set to the value of g:wibafoco_foo if this variable exists and
"   the len function applied to it returns a non-zero value,
" - or an exception being thrown if the len function applied to "bar" returns
"   zero,
" - or g:x being set to "bar".
"
" Arguments:
"
" #1 - ident
" Plugin related identifier ("foo" in the examples).
"
" #2 - default_value
" Default value for the plugin related parameter ("bar" in the examples).
"
" #3 - IsValid
" Reference (funcref) to a function designed to check the value of the plugin
" related parameter (function("len") in the examples). This function must
" return a non-zero value if the value of the plugin related parameter is valid
" and zero otherwise.
"
" Return value:
" Value of the plugin related parameter.
function s:Get(ident, default_value, IsValid)

    " Check the arguments.
    if !s:IsParamIdent(a:ident)
        throw "Invalid identifier"
    elseif type(a:IsValid) != type(function("tr"))
        throw "Last argument must be a funcref"
    endif

    if s:ExistsAsGlobal(a:ident)
        let l:ret = s:GetGlobal(a:ident)
    else
        let l:ret = a:default_value
    endif

    if !a:IsValid(l:ret)
        throw "Invalid custom " . a:ident . " parameter for " . s:plugin
                    \ .  " plugin"
    endif

    return l:ret
endfunction

" -----------------------------------------------------------------------------

" Checks that the argument is a dictionary containing valid parameters for a
" foldcolumn strategy. Such a dictionary contains the following keys:
" - win_width_threshold: a window width value (integer).
" - narrow_win_fdc: Value to be set to the foldcolumn option when the window
"   width is lower than or equal to the win_width_threshold value.
" - wide_win_fdc: Value to be set to the foldcolumn option when the window
"   width is greater than the win_width_threshold value.
"
" Arguments:
"
" #1 - d
" Anything.
"
" Return value:
" Non-zero if d is a dictionary containing valid parameters for a foldcolumn
" strategy.
function s:IsFdcStrategy(d)
    let l:ret = type(a:d) == type({})
    if l:ret
        let l:expected_keys = [
                    \ 'win_width_threshold',
                    \ 'narrow_win_fdc',
                    \ 'wide_win_fdc',
                    \ ]
        let l:match_count = 0
        for key in keys(a:d)
            if index(l:expected_keys, key) != -1
                let l:match_count += 1
            endif
        endfor
        let l:ret = l:match_count == len(l:expected_keys)
    endif
    if l:ret
        let l:ret = type(a:d['win_width_threshold']) == type(0)
        let l:ret = l:ret && type(a:d['narrow_win_fdc']) == type(0)
                    \ && a:d['narrow_win_fdc'] >= 0
        let l:ret = l:ret && type(a:d['wide_win_fdc']) == type(0)
                    \ && a:d['wide_win_fdc'] >= 0
    endif
    return l:ret
endfunction

" -----------------------------------------------------------------------------

" Computes the value to be given to the foldcolumn option, according to the
" strategy paramters and the window width given as arguments.
"
" Arguments:
"
" #1 - strategy
" dictionary containing the parameters for a foldcolumn strategy.
"
" #2 - win_width
" Window width.
"
" Return value:
" Value to be given to the foldcolumn option.
function s:Foldcolumn(strategy, win_width)

    " Check the arguments.
    if !s:IsFdcStrategy(a:strategy)
        throw "Invalid foldcolumn strategy dictionary"
    elseif type(a:win_width) != type(0)
        throw "Width of current window must be an integer"
    endif

    return a:win_width > a:strategy['win_width_threshold']
                \ ? a:strategy['wide_win_fdc']
                \ : a:strategy['narrow_win_fdc']
endfunction

" -----------------------------------------------------------------------------

" Sets the foldcolumn option in every window.
"
" Return value:
" 0
function s:WindoSetFoldcolumn()

    let l:strategy = s:Get("strategy", {
                \ 'win_width_threshold': 80,
                \ 'narrow_win_fdc': 0,
                \ 'wide_win_fdc': 1,
                \ }, function("s:IsFdcStrategy"))

    try

        " Store the number of the current window.
        let l:cur_win=winnr()

        " In each window, apply the foldcolumn strategy.
        windo let &l:foldcolumn = s:Foldcolumn(l:strategy, winwidth(0))

        " Come back to the window which was the current window when the
        " function has been called (the windo iteration mechanism always makes
        " the last window the current one).
        execute l:cur_win. "wincmd w"

    catch
        " Nothing particular is done if an error occurs in the try section. An
        " error occurs in the try section if the function is called while the
        " user is in the command window.
    endtry
endfunction

" -----------------------------------------------------------------------------

" Defines a map to a plugin function.
"
" Arguments:
"
" #1 - func
" Name of the plugin function (without the s: prefix).
"
" #2 - map
" Map (like "<Leader>S").
"
" Return value:
" 0
function s:DefineMapToPluginFunc(func, map)

    " Check the arguments
    if !s:IsFuncIdent(a:func)
        throw "Invalid function identifier"
    elseif !s:IsNonEmptyString(a:map)
        throw "Map must be a non-empty string"
    endif

    if !hasmapto('<Plug>' . s:plugin . a:func)
        execute "map <silent> <unique> " . a:map .
                    \ " <Plug>" . s:plugin . a:func
    endif
    execute "noremap <unique> <script> <Plug>" . s:plugin . a:func . " <SID>"
                \ . a:func
    execute "noremap <SID>" . a:func . " :call <SID>" . a:func . "()<CR>"
endfunction

" -----------------------------------------------------------------------------

" Associates a menu entry to a plugin function. The menu entry is placed in the
" Plugin|Wibafoco menu.
"
" Arguments:
"
" #1 - func
" Name of the plugin function (without the s: prefix).
"
" #2 - menu_entry
" Menu entry.
"
" Return value:
" 0
function s:DefineMenuForPluginFunc(func, menu_entry)

    " Check the arguments.
    if !s:IsFuncIdent(a:func)
        throw "Invalid function identifier"
    elseif !s:IsNonEmptyString(a:menu_entry)
        throw "Invalid menu entry"
    endif

    " Build menu entry location.
    let l:menu_location = "Plugin." . s:plugin . "."

    execute "noremenu <script> " . l:menu_location . escape(a:menu_entry, ' ')
                \ . " :call <SID>" . a:func . "()<CR>"
endfunction

" -----------------------------------------------------------------------------

" Associates a command to a plugin function. The name of the command is the
" name given as argument (name of the plugin function without the s: prefix)
" unless another name is given via the optional second argument.
"
" Arguments:
"
" #1 - func
" Name of the plugin function (without the s: prefix).
"
" #2 (optional)
" Command name.
"
" Return value:
" 0
function s:DefineCommandForPluginFunc(func, ...)

    " Check the arguments
    if !s:IsFuncIdent(a:func)
        throw "Invalid function identifier"
    elseif a:0 == 1 && !s:IsNonEmptyString(a:1)
        throw "Invalid command name"
    endif

    let a:name = a:0 == 1 ? a:1 : a:func
    if !exists(":" . a:name)
        execute "command " . a:name . " :call <SID>" . a:func . "()"
    endif
endfunction

" -----------------------------------------------------------------------------

" Issues autocmd statements, so that a plugin function is called automatically
" when some events occur.
"
" Arguments:
"
" #1 - func
" Name of the plugin function (without the s: prefix).
"
" #2 - event_list
" List of the events on which the function must be called.
"
" Return value:
" 0
function s:DefineAutocommandsForPluginFunc(func, event_list)

    " Check the arguments
    if !s:IsFuncIdent(a:func)
        throw "Invalid function identifier"
    else
        for event in a:event_list
            if !s:IsNonEmptyString(event)
                throw "An event cannot be an empty string"
            endif
        endfor
    endif

    for event in a:event_list
        execute "autocmd " . event . " * call <SID>" . a:func . "()"
    endfor

endfunction

" -----------------------------------------------------------------------------

let s:func = "WindoSetFoldcolumn"
call s:DefineMapToPluginFunc(s:func, "<Leader>fdc")
call s:DefineMenuForPluginFunc(s:func, "Update the 'foldcolumn' option")
call s:DefineCommandForPluginFunc(s:func, "Fdc")
call s:DefineAutocommandsForPluginFunc(s:func,
            \ ["VimEnter", "VimResized", "CursorHold"])

" Restore the value of cpoptions.
let &cpo = s:save_cpo

unlet s:save_cpo
