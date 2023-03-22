import JuliaSyntax
import JuliaSyntax: var"@K_str", Kind, Tokenize, tokenize
import .Tokenize: kind, untokenize

function getface(k::Kind)
    if k == K"String"; :julia_string
    elseif JuliaSyntax.is_string_delim(k); :julia_string
    elseif JuliaSyntax.is_number(k); :julia_number
    end
end

const MAX_PAREN_HIGHLIGHT_DEPTH = 6
const RAINBOW_DELIMITERS_ENABLED = Ref(true)

function highlight(tokens)
    highlighted = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    lastk = K"None"
    last2k = K"None"
    parendepth, bracketdepth, curlydepth = 0, 0, 0
    for (; head::JuliaSyntax.SyntaxHead, range::UnitRange{UInt32}) in tokens
        kind = head.kind
        face = if kind == K"Identifier"
            if lastk == K":" && !JuliaSyntax.is_number(last2k) &&
                last2k ∉ (K"Identifier", K")", K"]", K"end", K"'")
                highlighted[end] = (highlighted[end][1], :face => :julia_symbol)
                :julia_symbol
            elseif lastk == K"::"; :julia_type
            else :julia_identifier end
        elseif kind == K"@"; :julia_macro
        elseif kind == K"MacroName"; :julia_macro
        elseif kind == K"StringMacroName"; :julia_macro
        elseif kind == K"CmdMacroName"; :julia_macro
        elseif kind == K"::"; :julia_type
        elseif kind == K"nothing"; :julia_nothing
        elseif kind == K"Comment"; :julia_comment
        elseif kind == K"String"; :julia_string
        elseif JuliaSyntax.is_string_delim(kind); :julia_string_delim
        elseif kind == K"CmdString"; :julia_cmdstring
        elseif kind == K"`" || kind == K"```"; :julia_cmdstring
        elseif kind == K"Char"
            lastk == K"'" &&
                (highlighted[end] = (highlighted[end][1], :face => :julia_char_delim))
            :julia_char
        elseif kind == K"'" && lastk == K"Char"; :julia_char_delim
        elseif kind == K"true" || kind == K"false"; :julia_bool
        elseif JuliaSyntax.is_number(kind); :julia_number
        elseif JuliaSyntax.is_prec_assignment(kind); :julia_assignment
        elseif JuliaSyntax.is_prec_comparison(kind); :julia_comparison
        elseif JuliaSyntax.is_operator(kind); :julia_operator
        elseif JuliaSyntax.is_keyword(kind); :julia_keyword
        elseif JuliaSyntax.is_error(kind); :julia_error
        elseif !RAINBOW_DELIMITERS_ENABLED[] && kind == K"("
            lastk == K"Identifier" &&
                (highlighted[end] = (highlighted[end][1], :face => :julia_funcall))
            :julia_parenthetial
        elseif !RAINBOW_DELIMITERS_ENABLED[] && kind in (K")", K"[", K"]", K"{", K"}")
            :julia_parenthetial
        elseif kind == K"("
            lastk == K"Identifier" &&
                (highlighted[end] = (highlighted[end][1], :face => :julia_funcall))
            name = Symbol("julia_rainbow_paren_$(parendepth+1)")
            parendepth = mod(parendepth + 1, MAX_PAREN_HIGHLIGHT_DEPTH)
            name
        elseif kind == K")"
            parendepth = mod(parendepth - 1, MAX_PAREN_HIGHLIGHT_DEPTH)
            Symbol("julia_rainbow_paren_$(parendepth+1)")
        elseif kind == K"["
            name = Symbol("julia_rainbow_bracket_$(bracketdepth+1)")
            bracketdepth = mod(bracketdepth + 1, MAX_PAREN_HIGHLIGHT_DEPTH)
            name
        elseif kind == K"]"
            bracketdepth = mod(bracketdepth - 1, MAX_PAREN_HIGHLIGHT_DEPTH)
            Symbol("julia_rainbow_bracket_$(bracketdepth+1)")
        elseif kind == K"{"
            name = Symbol("julia_rainbow_curly_$(curlydepth+1)")
            curlydepth = mod(curlydepth + 1, MAX_PAREN_HIGHLIGHT_DEPTH)
            name
        elseif kind == K"}"
            curlydepth = mod(curlydepth - 1, MAX_PAREN_HIGHLIGHT_DEPTH)
            Symbol("julia_rainbow_curly_$(curlydepth+1)")
        end
        isnothing(face) || push!(highlighted, (range, :face => face))
        last2k, lastk = lastk, kind
    end
    highlighted
end

highlight(str::AbstractString) =
    StyledString(str, highlight(tokenize(str)))

function highlight(buf::IOBuffer)
    pos = position(buf)
    seekstart(buf)
    out = StyledString(String(take!(copy(buf))), highlight(tokenize(buf)))
    seek(buf, pos)
    out
end

highlight(buf::IOContext) = highlight(buf.io)
