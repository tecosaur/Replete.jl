# TODO now the basic test is done, make a cleaner,
# more general model.

# Creating a fancy selector

const CandidateList{V, S <: AbstractString} = AbstractVector{Tuple{Int, V, S}}

struct Selector{V, S}
    prompt::AbstractString
    candidates::CandidateList{V, S}
    filter::Function
    height::Int
    format::Function
    preview::UnitRange{Int}
    help::Union{AbstractString, Nothing}
    reverse::Bool
end

mutable struct SelectionState{V, S}
    selector::Selector{V, S}
    filtered::CandidateList{V, S}
    selection::Vector{Int}
    hover::Int
    window::Tuple{Int, Int}
    query::AbstractString
end

Selector(prompt::AbstractString, candidates::CandidateList, filter::Function, height::Int;
         format::Function=identity, preview::Union{UnitRange{Int}, Bool}=false,
         help::Union{AbstractString, Nothing}=nothing, reverse::Bool=false) =
             Selector(prompt, candidates, filter, height, format,
                      if preview === true
                          (1 + height ÷ 4):(1 + height ÷ 3)
                      elseif preview === false
                          0:0
                      else preview end,
                      help, reverse)

Selector(prompt::AbstractString, candidates::AbstractVector{Tuple{V, S}},
         args...; kwargs...) where { V, S <: AbstractString } =
    Selector(prompt, Vector{Tuple{Int, V, S}}([
        (i, v, s) for (i, (v, s)) in enumerate(candidates)]),
             args...; kwargs...)

SelectionState(sel::Selector{V, S}) where {V, S} =
    SelectionState(sel, sel.candidates,
                   Int[], if sel.reverse 1 else length(sel.candidates) end,
                   if sel.reverse
                       (1, min(sel.height, length(sel.candidates)))
                   else (max(1, length(sel.candidates) - sel.height + 1),
                         length(sel.candidates)) end,
                   "")

# The prompt

function redisplay(ss::SelectionState, prompt_state)
    query = REPL.LineEdit.input_string(prompt_state)
    # In case of help
    if !isnothing(ss.selector.help) && query == "?"
        ss.query = "?"
        display_help(stdout, ss, prompt_state)
        return
    end
    out = IOContext(IOBuffer(), stdout)
    width = displaysize(stdout)[2]
    # Clear space
    print(out, "\e[1G\e[J")
    updatefilter(ss, query)
    # The preview
    preview_buf = IOContext(IOBuffer(), out)
    preview_lines = selection_preview(preview_buf, ss, width)
    # The candidates
    candidates_buf = IOContext(IOBuffer(), out)
    num_candidates = ss.selector.height - preview_lines - 1
    updatewindow(ss, num_candidates)
    candidate_lines = selection_candidates(candidates_buf, ss, width)
    # The prompt
    selection_prompt(out, ss, width)
    # Print candidates
    write(out, take!(candidates_buf.io))
    # Fill blank lines
    if (gap = ss.selector.height - candidate_lines - preview_lines - 1) > 0
        print(out, (' '^width * '\n')^gap)
    end
    # Print preview
    write(out, take!(preview_buf.io))
    # Move cursor back into the prompt
    print(out, "\e[", string(ss.selector.height), 'A')
    print(out, "\e[", textwidth(ss.selector.prompt) + position(prompt_state.input_buffer) + 1, 'G')
    # Done
    write(stdout, take!(out.io))
    flush(stdout)
end

function display_help(io::IO, ss::SelectionState, prompt_state)
    out = IOContext(IOBuffer(), io)
    print(out, "\e[1G\e[J")
    selection_prompt(out, ss, showinfo=false)
    print(out, ss.selector.help)
    # Move cursor back into the prompt
    print(out, "\e[", string(length(split(ss.selector.help, '\n'))), 'A')
    print(out, "\e[", textwidth(ss.selector.prompt) + position(prompt_state.input_buffer) + 1, 'G')
    # Done
    write(stdout, take!(out.io))
    flush(stdout)
end

function updatefilter(ss::SelectionState, query::String)
    if ss.query != query
        ss.query = query
        ss.filtered = ss.selector.filter(ss)
        ss.window = (max(1, length(ss.filtered) - ss.selector.height), length(ss.filtered))
        ss.hover = clamp(ss.hover, ss.window...)
    end
    ss.hover = clamp(ss.hover, 1, length(ss.filtered))
end

function updatewindow(ss::SelectionState, maxlines::Int=0)
    if maxlines > 0 && (window_gap = maxlines - (ss.window[2] - ss.window[1])) != 0
        ss.window = if ss.window[1] > window_gap
            (ss.window[1] - window_gap, ss.window[2])
        else
            (1, min(maxlines, length(ss.filtered)))
        end
    end
    if ss.hover < ss.window[1]
        steps = ss.window[1] - ss.hover
        ss.window = ss.window .- steps
    elseif ss.hover > ss.window[2]
        steps = ss.hover - ss.window[2]
        ss.window = ss.window .+ steps
    end
end

# TODO generalise
function selection_prompt(io::IO, ss::SelectionState, width::Int=displaysize(io)[2];
                          showinfo::Bool=true)
    prompt = styledstring(ss.selector.prompt, ss.query)
    print(io, "\e[2K", prompt)
    # Info token
    info = StyledString("[$(ss.hover)/$(length(ss.filtered))]",
                        :face => :repl_history_search_results)
    if showinfo && textwidth(prompt) + textwidth(info) < width
        print(io, ' '^(width - textwidth(prompt) - textwidth(info) - 1),
              info, '\n')
    else print(io, '\n') end
end

function selection_candidates(io::IO, ss::SelectionState, width::Int=displaysize(io)[2])
    marker_selected = StyledChar('⬤', :face => :repl_history_search_selected)
    marker_unselected = StyledChar('∘', :face => :repl_history_search_unselected)
    for (i, cand) in zip(ss.window[1]:ss.window[2], view(ss.filtered, ss.window[1]:ss.window[2]))
        print(io, ' ',
              ifelse(first(cand) in ss.selection,
                     marker_selected, marker_unselected),
              ' ')
        truncated = map(c -> ifelse(c in ('\n', '\r', '\t'), ' ', c),
                        Iterators.take(last(cand), width))
        linewidth = mapreduce(textwidth, +, truncated)
        line = if linewidth <= width-4
            rpad(ss.selector.format(String(truncated)), width-3)
        else
            while linewidth > width-4
                linewidth -= textwidth(pop!(truncated))
            end
            styledstring(rpad(ss.selector.format(String(truncated)), width-4),
                         StyledChar('…', :face => :shadow))
        end
        if i == ss.hover
            print(io, face!(line, :region))
        else
            print(io, line)
        end
        print(io, '\n')
    end
    ss.window[2] - ss.window[1]
end

function selection_preview(io::IO, ss::SelectionState, width::Int=displaysize(io)[2])
    if first(ss.selector.preview) <= 2
        print(io, (' '^width * '\n')^first(ss.selector.preview))
        first(ss.selector.preview)
    else
        above = StyledString(string('╭', '─'^(width-2), '╮'), :face => :shadow)
        below = StyledString(string('╰', '─'^(width-2), '╯'), :face => :shadow)
        bar = StyledChar('│', :face => :shadow)
        print(io, above, '\n')
        content = IOContext(IOBuffer(), io)
        hover_region = nothing
        if !isempty(ss.selection)
            for sel in getindex.(Ref(ss.selector.candidates), ss.selection)
                if !isempty(ss.filtered) && first(sel) == first(ss.filtered[ss.hover])
                    hover_region = (content.size+1):(content.size+length(last(sel)))
                end
                println(content, last(sel))
            end
        elseif !isempty(ss.filtered)
            println(content, last(ss.filtered[ss.hover]))
        end
        str = if content.io.size == 0 "" else # Strip trailing newline.
            ss.selector.format(String(take!(content.io)[1:end-1])) end
        isnothing(hover_region) ||
            face!(str, hover_region, :region)
        lines = split(str, '\n') |> collect
        function printline(line)
            if textwidth(line) <= width-4
                print(io, bar, ' ', rpad(line, width-4), ' ', bar, '\n')
            else
                len = mapreduce(length, +, Iterators.take(line.string, width-4))
                trunc = line[firstindex(line):prevind(line, firstindex(line)+len)]
                print(io, bar, ' ', trunc, StyledChar('…', :face => :shadow), bar, '\n')
            end
        end
        # Determine the total number of lines to show
        nlines = clamp(length(lines), first(ss.selector.preview)-2, last(ss.selector.preview)-2)
        if isempty(lines)
        elseif length(lines) <= nlines
            printline.(lines)
        else
            half1 = (nlines - 1) ÷ 2
            half2 = nlines - half1 - 2
            printline.(lines[1:half1])
            nhidden = length(lines) - nlines + 1
            printline(StyledString("⋮ $nhidden lines hidden",
                                   :face => :julia_comment,
                                   :face => :italic))
            printline.(lines[end-half2:end])
        end
        # Pad with blank lines to ensure the minimum line count
        if length(lines) < first(ss.selector.preview) - 2
            blank = styledstring(bar, ' '^(width-2), bar, '\n')
            print(io, blank^(nlines - length(lines)))
        end
        print(io, below)
        nlines + 2
    end
end

function select_keymap(ss::SelectionState)
    REPL.LineEdit.keymap([
        Dict{Any, Any}(
            # Up Arrow
            "\e[A" => (_...) -> (ss.hover -= 1; :ignore),
            # Down Arrow
            "\e[B" => (_...) -> (ss.hover += 1; :ignore),
            "^P" => (_...) -> (ss.hover -= 1; :ignore),
            "^N" => (_...) -> (ss.hover += 1; :ignore),
            # Tab
            '\t' => function (s::REPL.LineEdit.MIState, o...)
                if REPL.LineEdit.complete_line(REPL.LineEdit.state(s), s.key_repeats, s.active_module)
                    # FIXME do something to fix the position jumping
                else
                    index = first(ss.filtered[ss.hover])
                    if index ∈ ss.selection
                        deleteat!(ss.selection, findfirst(index .== ss.selection))
                    else
                        push!(ss.selection, index)
                    end
                    ss.hover += 1
                end
                :ignore
            end,
            # Backspace
            '\b' => function (s::REPL.LineEdit.MIState, o...)
                REPL.LineEdit.edit_backspace(
                    REPL.LineEdit.buffer(s),
                    REPL.LineEdit.options(s).backspace_align,
                    REPL.LineEdit.options(s).backspace_adjust)
                :ok
            end,
            # Delete
            "\e[3~" => function (s::REPL.LineEdit.MIState, o...)
                REPL.LineEdit.edit_delete(REPL.LineEdit.buffer(s))
                :ok
            end,
            # Page up
            "\e[5~" => (_...) -> (ss.hover -= (ss.window[2] - ss.window[1]); :ignore),
            # Page down
            "\e[6~" => (_...) -> (ss.hover += (ss.window[2] - ss.window[1]); :ignore),
            "\e<" => (_...) -> (ss.hover = 1; :ignore),
            "\e>" => (_...) -> (ss.hover = length(ss.filtered); :ignore),
            "^C" => Returns(:abort),
            "^S" => Returns(:save),
        ),
        REPL.LineEdit.default_keymap,
        REPL.LineEdit.escape_defaults])
end

function select_prompt(ss::SelectionState)
    REPL.LineEdit.Prompt(
        ": ", # prompt
        "\e[34m", "\e[0m", # prompt_prefix, prompt_suffix
        "", "", "", # output_prefix, output_prefix_prefix, output_prefix_suffix
        select_keymap(ss), # keymap
        nothing, # repl
        REPL.LatexCompletions(), # complete
        _ -> true, # on_enter
        () -> nothing, # on_done
        REPL.LineEdit.EmptyHistoryProvider(), # hist
        false) # sticky
end

function select_run_prompt(ss::SelectionState)
    term = REPL.Terminals.TTYTerminal(
        get(ENV, "TERM", Sys.iswindows() ? "" : "dumb"),
        stdin, stdout, stderr)
    prompt = select_prompt(ss)
    #
    interface = REPL.LineEdit.ModalInterface([prompt])
    istate = REPL.LineEdit.init_state(term, interface)
    pstate = istate.mode_state[prompt]
    #
    Base.reseteof(term)
    REPL.LineEdit.raw!(term, true)
    REPL.LineEdit.enable_bracketed_paste(term)
    try
        pstate.ias = REPL.LineEdit.InputAreaState(0, 0)
        while true
            redisplay(ss, pstate)
            kmap = REPL.LineEdit.keymap(pstate, prompt)
            matchfn = REPL.LineEdit.match_input(kmap, istate)
            kdata = REPL.LineEdit.keymap_data(pstate, prompt)
            status = matchfn(istate, kdata)
            if status === :ok
            elseif status === :ignore
                istate.last_action = istate.current_action
            elseif status === :done
                print("\e[F")
                return if isempty(ss.selection)
                    ss.filtered[ss.hover]
                else
                    ss.selector.candidates[ss.selection]
                end
            elseif status === :save
                print("\e[1G\e[J")
                REPL.LineEdit.raw!(term, false) &&
                    REPL.LineEdit.disable_bracketed_paste(term)
                print("File: ")
                filename = readline()
                data = join(if isempty(ss.selection)
                    (ss.filtered[ss.hover],)
                else
                    ss.selector.candidates[ss.selection]
                end .|> last, '\n')
                write(filename, data)
                print("\e[F\e[2KWrote history selection to ", filename, '\n')
                return nothing
            else
                return nothing
            end
        end
    finally
        print("\e[1G\e[J")
        REPL.LineEdit.raw!(term, false) &&
            REPL.LineEdit.disable_bracketed_paste(term)
    end
end

# Filtering rules

const FILTER_SEPERATOR = ';'
const FILTER_ESCAPE = '\\'
const FILTER_PREFIXES = ('!', '`', '=', '^', '~', '>')

filter_not(pattern::AbstractString, candidates::CandidateList) =
    filter(cand -> !occursin(pattern, last(cand)), candidates)

function filter_regex(pattern::AbstractString, candidates::CandidateList)
    try
        rx = Regex(pattern, ifelse(any(isuppercase, pattern), "", "i"))
        filter(cand -> !isnothing(match(rx, last(cand))), candidates)
    catch _ # PCRE compilation error
        candidates
    end
end

function filter_initialism(pattern::AbstractString, candidates::CandidateList)
    rx = Regex(join((string("\\Q", p, "\\E\\w+") for p in pattern), "\\s+"),
               ifelse(any(isuppercase, pattern), "", "i"))
    filter(cand -> !isnothing(match(rx, last(cand))), candidates)
end

filter_literal(pattern::AbstractString, candidates::CandidateList) =
    filter(cand -> occursin(pattern, last(cand)), candidates)

function filter_fuzzy(pattern::AbstractString, candidates::CandidateList)
    rxs = [Regex(join(string.("\\Q", collect(word), "\\E"), ".*"))
           for word in split(pattern)]
    for rx in rxs
        candidates = filter(cand -> !isnothing(match(rx, last(cand))), candidates)
    end
    candidates
end

function filter_mode(pattern::AbstractString, candidates::CandidateList)
    filter(cand -> startswith(string(cand[2]), pattern), candidates)
end

function filter_unordered(pattern::AbstractString, candidates::CandidateList)
    words = [Regex(string("\\Q", word, "\\E"), ifelse(any(isuppercase, pattern), "", "i"))
             for word in split(pattern)]
    for word in words
        candidates = filter(cand -> !isnothing(match(word, last(cand))), candidates)
    end
    candidates
end

function filterdispatch(pattern::AbstractString, candidates::CandidateList)
    search = SubString(pattern, nextind(pattern, firstindex(pattern)):lastindex(pattern))
    if startswith(pattern, '!')
        filter_not(search, candidates)
    elseif startswith(pattern, '`')
        filter_initialism(search, candidates)
    elseif startswith(pattern, '=')
        filter_literal(search, candidates)
    elseif startswith(pattern, '^')
        filter_regex(pattern, candidates)
    elseif startswith(pattern, '~')
        filter_fuzzy(search, candidates)
    elseif startswith(pattern, '>')
        filter_mode(search, candidates)
    elseif startswith(pattern, FILTER_ESCAPE)
        filter_unordered(search, candidates)
    else
        filter_unordered(pattern, candidates)
    end
end

function filterpatterns(patterns::Vector{<:AbstractString}, candidates::CandidateList)
    for pattern in patterns
        candidates = filterdispatch(pattern, candidates)
    end
    candidates
end

"""
    splitpatterns(text::AbstractString)
Split `text` into multiple patterns seperated by `$FILTER_SEPERATOR`.
Literal `$FILTER_SEPERATOR` characters can be escaped by `$FILTER_ESCAPE`.
"""
function splitpatterns(text::S; keepempty::Bool=false) where {S <: AbstractString}
    patterns = Vector{SubString{S}}()
    patstart = firstindex(text)
    escaped = false
    for (pos, char) in zip(eachindex(text), text)
        if escaped
            escaped = false
        elseif char == FILTER_ESCAPE
            escaped = true
        elseif char == FILTER_SEPERATOR
            nextpos = nextind(text, pos)
            if pos != patstart || keepempty # Ignore double seperator
                push!(patterns, SubString(text, patstart:prevind(text, pos)))
            end
            patstart = nextpos
        end
    end
    if patstart <= lastindex(text) ||
        (keepempty && !isempty(text) && !escaped && last(text) == FILTER_SEPERATOR)
        push!(patterns, SubString(text, patstart:lastindex(text)))
    end
    patterns
end

const FILTER_HELPSTRING = S"""
 {bold:Interactive history search}

 Enter a seach term at the prompt, and see matching candidates.
 A search term that is {italic:just} '{repl_history_search_prefix:?}' brings up this help page.

 Different search modes are availble via prefixes, as follows:
 • {repl_history_search_prefix:=} looks for exact matches
 • {repl_history_search_prefix:!} {italic:excludes} exact matches
 • {repl_history_search_prefix:^} performs a regexp search
 • {repl_history_search_prefix:~} looks for fuzzy matchs
 • {repl_history_search_prefix:>} looks for a particular REPL mode
 • {repl_history_search_prefix:`} looks for an initialism

 You can also combine multiple search modes with the \
seperator '{repl_history_search_seperator:$FILTER_SEPERATOR}',
 for example:
   {repl_history_search_prefix:^}foo{repl_history_search_seperator:$FILTER_SEPERATOR}\
{repl_history_search_prefix:`}bar{repl_history_search_seperator:$FILTER_SEPERATOR}\
{repl_history_search_prefix:>}shell\

 will look for history entries that start with "foo",
 contain "b... a... r...", and is are a shell history entry.
 A literal '$FILTER_SEPERATOR' can be escaped by being prefixed with '$FILTER_ESCAPE'.
"""

# The history selector

const HISTORY_PROMPT = StyledString(" > ", :face => :repl_history_search_prompt)

function historyselector()
    function filterhistory(ss::SelectionState)
        patterns = splitpatterns(ss.query)
        ss.query = join(Iterators.map(splitpatterns(ss.query, keepempty=true)) do pat
                            if !isempty(pat) && first(pat) in FILTER_PREFIXES
                                StyledString(pat, 1:1, :face => :repl_history_search_prefix)
                            else pat end
                        end,
                        StyledString(string(FILTER_SEPERATOR), :face => :repl_history_search_seperator))
        filterpatterns(patterns, ss.selector.candidates)
    end
    height = (displaysize(stdout)[1] * 2) ÷ 4
    preview = if height > 12
        ((height - 1) ÷ 3):(2*(height - 1) ÷ 3)
    else 0:0 end
    Selector(HISTORY_PROMPT, history_entries(), filterhistory, height;
             format=highlight, preview, help = FILTER_HELPSTRING)
end

function history_entries()::CandidateList
    i = 0
    entries = Vector{Tuple{Int, Symbol, String}}()
    hist = open(REPL.find_hist_file(), "r")
    entry_text = IOBuffer()
    entry_mode = :unknown
    for line in Iterators.peel(eachline(hist)) |> last
        if startswith(line, "# time: ")
            push!(entries, (i+=1, entry_mode, String(@view take!(entry_text)[1:end-1])))
        elseif startswith(line, "# mode: ")
            entry_mode = Symbol(view(line, 9:lastindex(line)))
        elseif startswith(line, '\t')
            write(entry_text, view(line, 2:lastindex(line)))
            write(entry_text, '\n')
        else
            @error "Malformed history line: $line"
        end
    end
    close(hist)
    entries
end

function selecthistory()
    select_run_prompt(SelectionState(historyselector()))
end
