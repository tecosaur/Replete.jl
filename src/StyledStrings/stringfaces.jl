# Face/StyledString integration

"""
    getface(s::StyledString, i::Integer)

Get the merged [`Face`](@ref) that applies to `s` at index `i`.
"""
getface(s::StyledString, i::Integer) =
    getface(textproperties(s, i))

"""
    getface(c::StyledChar)

Get the merged [`Face`](@ref) that applies to `c`.
"""
getface(c::StyledChar) = getface(c.properties)

"""
    face!(s::Union{<:StyledString, <:SubString{<:StyledString}},
          [range::UnitRange{Int},] face::Union{Symbol, Face})

Apply `face` to `s`, along `range` if specified, or the whole of `s`.
"""
face!(s::Union{<:StyledString, <:SubString{<:StyledString}},
      range::UnitRange{Int}, face::Union{Symbol, Face}) =
          textproperty!(s, range, :face, face)

face!(s::Union{<:StyledString, <:SubString{<:StyledString}},
      face::Union{Symbol, Face}) =
          textproperty!(s, firstindex(s):lastindex(s), :face, face)

# Style macro

"""
    @S_str -> StyledString

Construct a styled string. Within the string, `{<specs>:<content>}` structures
apply the formatting to `<content>`, according to the list of comma-separated
specifications `<specs>`. Each spec can either take the form of a face name,
or a `key=value` pair. The value must be wrapped by `{...}` should it contain
any of the characters `,=:{}`.

String interpolation with `\$` functions in the same way as regular strings.

# Example

```julia
S"The {bold:{italic:quick} brown fox} jumped over \
the {link={https://en.wikipedia.org/wiki/Laziness}:lazy} dog"
```
"""
macro S_str(raw_content::String)
    parts = Any[]
    content = unescape_string(raw_content, ('{', '}', '$', '\n'))
    cvec = Vector{UInt8}(content)
    s = Iterators.Stateful(zip(eachindex(content), content))
    offset = 0
    point = 1
    escape = false
    thisstringstyles = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    activestyles = Vector{Tuple{Int, Pair{Symbol, Any}}}()
    function addpart(stop::Int)
        str = String(cvec[point:stop+offset+ncodeunits(content[stop])-1])
        if isempty(thisstringstyles) && isempty(activestyles)
            push!(parts, str)
        else
            styles = vcat(
                map((start, prop)::Tuple ->
                    ((start - point):(stop - point + offset + 1), prop),
                    filter((start, _)::Tuple -> start <= stop + offset,
                           activestyles)),
                map((range, prop)::Tuple -> (range .- point, prop),
                    thisstringstyles)) |>
                        Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}
            push!(parts, StyledString(str, styles))
            empty!(thisstringstyles)
        end
        point = nextind(content, stop) + offset
    end
    function addpart(start::Int, expr, stop::Int)
        if point < start
            addpart(start)
        end
        if isempty(activestyles)
            push!(parts, expr)
        else
            push!(parts,
                  :(StyledString(string($expr), $(last.(activestyles)...))))
            activestyles = map((_, prop)::Tuple ->
                (nextind(content, stop + offset), prop),
                               activestyles)
        end
    end
    for (i, char) in s
        if char == '\\'
            escape = true
        elseif escape
            if char in ('{', '}', '$')
                deleteat!(cvec, i + offset - 1)
                offset -= 1
            elseif char == '\n'
                deleteat!(cvec, i+offset-1:i+offset)
                offset -= 2
            end
            escape = false
        elseif char == '$'
            # Interpolation
            expr, nexti = Meta.parseatom(content, i + 1)
            deleteat!(cvec, i + offset)
            offset -= 1
            for _ in 1:min(length(s), nexti - i - 1)
                popfirst!(s)
            end
            addpart(i, expr, nexti)
            point = nexti + offset
        elseif char == '{'
            # Property declaration parsing and application
            properties = true
            hasvalue = false
            while properties
                if !isnothing(peek(s)) && last(peek(s)) == '('
                    # Inline face
                    popfirst!(s)
                    specstr = Iterators.takewhile(c -> last(c) != ')', s) |>
                        collect .|> last |> String
                    spec = map(split(specstr, r", *")) do spec
                        kv = split(spec, '=', limit=2)
                        if length(kv) == 2
                            kv[1] => @something(tryparse(Bool, kv[2]),
                                                String(kv[2]))
                        else "" => "" end
                    end |> Dict
                    push!(activestyles, (nextind(content, i + offset),
                                         :face => convert(Face, spec)))
                    if isnothing(peek(s)) || last(popfirst!(s)) != ','
                        properties = false
                    end
                else
                    # Face symbol or key=value pair
                    key = Iterators.takewhile(
                        function(c)
                            if last(c) == ':' # Start of content
                                properties = false
                            elseif last(c) == '=' # Start of value
                                hasvalue = true
                                false
                            elseif last(c) == ',' # Next key
                                false
                            else true end
                        end, s) |> collect .|> last |> String
                    if isempty(key)
                        break
                    elseif hasvalue
                        hasvalue = false
                        value = if !isnothing(peek(s))
                            if last(peek(s)) == '{'
                                # Grab {}-wrapped value
                                popfirst!(s)
                                isescaped = false
                                val = Vector{Char}()
                                while (next = popfirst!(s)) |> !isnothing
                                    (_, c) = next
                                    if isescaped && c ∈ ('\\', '}')
                                        push!(val, c)
                                    elseif isescaped
                                        push!(val, '\\', c)
                                    elseif c == '}'
                                        break
                                    else
                                        push!(val, c)
                                    end
                                end
                                String(val)
                            else
                                # Grab up to next value, or start of content.
                                Iterators.takewhile(
                                    function (c)
                                        if last(c) == ':'
                                            properties = false
                                        elseif last(c) == ','
                                            false
                                        else true end
                                    end, s) |> collect .|> last |> String
                            end
                        end
                        push!(activestyles,
                              (nextind(content, i + offset),
                               Symbol(key) => value))
                    else
                        # No value, hence a face symbol
                        push!(activestyles,
                              (nextind(content, i + offset),
                               :face => Symbol(key)))
                    end
                end
            end
            # Adjust cvec/offset based on how much the index
            # has been incremented in the processing of the
            # style declaration(s).
            if !isnothing(peek(s))
                nexti = first(peek(s))
                deleteat!(cvec, i+offset:nexti+offset-1)
                offset -= nexti - i
            end
        elseif char == '}'
            # Close off most recent active style
            start, prop = pop!(activestyles)
            push!(thisstringstyles, (start:i+offset, prop))
            deleteat!(cvec, i + offset)
            offset -= 1
        end
    end
    # Ensure that any trailing unstyled content is added
    if point <= lastindex(content) + offset
        addpart(lastindex(content))
    end
    if length(parts) == 1
        first(parts)
    else
        :(styledstring($((map(esc, parts))...)))
    end
end
