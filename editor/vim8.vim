
let s:job = 0
let s:channel = 0
let s:active = 0

let s:last_kind = "any"
let s:last_label = "target"

let s:highlights = []

function! s:ModeBegin()
    if s:active
        echo "already refactoring"
        return
    end

    let s:job = job_start("./run-interact.sh",
                \ {"out_cb": "IdiomizeOutputHandler",
                \  "err_cb": "IdiomizeErrorHandler",
                \
                \  "out_io": "buffer",
                \  "out_name": "<stdout>",
                \  "out_modifiable": 0,
                \  "out_msg": 1,
                \
                \  "err_io": "buffer",
                \  "err_name": "<stderr>",
                \  "err_modifiable": 0,
                \  "err_msg": 1,
                \  })
    let s:channel = job_getchannel(s:job)
    let s:active = 1
    echo "opened" s:channel
endfunction

function! s:ModeEnd()
    echo "mode end!"
    if !s:active
        return
    endif
    call job_stop(s:job)
    call s:ClearHighlights()

    let s:job = 0
    let s:channel = 0
    let s:active = 0
endfunction


nnoremap <Leader>r :call <SID>ModeBegin()<CR>
nnoremap <Leader>q :call <SID>ModeEnd()<CR>
nnoremap <Leader>m :call <SID>DoMark()<CR>
nnoremap <Leader>M :call <SID>DoMarkNamed()<CR>
nnoremap <Leader>c :call <SID>DoCommand()<CR>
nnoremap <Leader>f :call <SID>DoFormat()<CR>
nnoremap <Leader>s :source %<CR>

function! s:Send(json)
    if !s:active
        echo "not refactoring"
        return
    end

    let msg = json_encode(a:json)
    call ch_sendraw(s:channel, msg . "\n")
endfunction

function! s:Mark(kind, label)
    call s:SendAvailableBuffers()
    call s:Send({
                \ "msg": "add-mark",
                \ "file": expand("%"),
                \ "line": line("."),
                \ "col": col(".") - 1,
                \ "kind": a:kind,
                \ "label": a:label,
                \ })
endfunction


function! s:DoMark()
    call s:Mark("any", "target")
endfunction

function! s:DoMarkNamed()
    let kind = input("Node kind? ", s:last_kind)
    let label = input("Label? ", s:last_label)
    call s:Mark(kind, label)

    let s:last_kind = kind
    let s:last_label = label
endfunction

function! s:DoCommand()
    let cmd = input("Command: ")
    let parts = split(cmd)

    call s:SendAvailableBuffers()
    call s:Send({
                \ "msg": "run-command",
                \ "name": parts[0],
                \ "args": parts[1:],
                \ })
endfunction

function! s:DoFormat()
    let cur_line = line(".")
    %!./run-fmt.sh
    exec cur_line
endfunction


function! s:SendAvailableBuffers()
    let buf_files = []
    for info in getbufinfo()
        call add(buf_files, info["name"])
    endfor

    call s:Send({
                \ "msg": "set-buffers-available",
                \ "files": buf_files,
                \ })
endfunction

function! s:SetMark(mark, file, line, col)
    let nr = bufnr(a:file)
    if nr == -1
        return
    endif
    call setpos(a:mark, [nr, a:line, a:col, a:col])
endfunction

function! IdiomizeOutputHandler(channel, msg)
    let json = json_decode(a:msg)
    if json["msg"] == "mark-info"
        call s:SetMark("'<", json["file"], json["start_line"], json["start_col"] + 1)
        call s:SetMark("'>", json["file"], json["end_line"], json["end_col"] + 1)
        "normal gv
        call s:Highlight(json["start_line"], json["start_col"] + 1,
                    \ json["end_line"], json["end_col"] + 1)
        echo "Marked node as " . join(json["labels"], ", ")
    elseif json["msg"] == "node-list"
        call s:ClearHighlights()
        for id in json["nodes"]
            call s:Send({
                        \ "msg": "get-mark-info",
                        \ "id": id,
                        \ })
        endfor
    elseif json["msg"] == "get-buffer-text"
        let nr = bufnr(json["file"])
        let lines = getbufline(nr, 1, "$")
        call s:Send({
                    \ "msg": "buffer-text",
                    \ "file": json["file"],
                    \ "content": join(lines, "\n"),
                    \ })
    elseif json["msg"] == "new-buffer-text"
        let nr = bufnr(json["file"])
        let lines = split(json["content"], "\n")
        let cur_nr = bufnr("%")
        let cur_line = line(".")

        if nr != -1
            if nr != cur_nr
                exec 'buffer ' . nr
            endif
            %delete _
            call append(0, lines)
            if nr != cur_nr
                exec 'buffer ' . cur_nr
            else
                exec cur_line
            endif
        endif
    elseif json["msg"] == "error"
        redraw
        echohl ErrorMsg
        echom json["text"]
        echohl None
    else
        echo "unrecognized message type" json["msg"]
    endif
endfunction

function! IdiomizeErrorHandler(channel, msg)
    "echohl WarningMsg
    "echom a:msg
    "echohl None
endfunction


hi default IdiomizeMarkedNode ctermbg=52

function! s:Highlight(line1, col1, line2, col2)
    let pat = '\%' . a:line1 . 'l\%' . a:col1 . 'c\_.*' .
                \ '\%' . a:line2 . 'l\%' . a:col2 . 'c'
    echom "highlight" pat
    let hl = matchadd("IdiomizeMarkedNode", pat)
    call add(s:highlights, hl)
endfunction

function! s:ClearHighlights()
    for hl in s:highlights
        call matchdelete(hl)
    endfor
    let s:highlights = []
endfunction
