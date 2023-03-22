"""
    StyledString{S <: AbstractString} <: AbstractString

A string annotated by properties.

More specifically, this is a thin wrapper around any other [`AbstractString`](@ref),
which adds arbitrary named annotations to regions of the wrapped string.

## Constructors

```julia
StyledString(s::S) -> StyledString{S}
StyledString(s::S, props::Pair{Symbol, <:Any}...)
StyledString(s::S, region::UnitRange{Int}, props::Pair{Symbol, <:Any}...)
StyledString(s::S, properties::Vector{Tuple{UnitRange{Int}, Pair{Symbol, <:Any}}})
```

A StyledString can also be created with [`styledstring`](@ref), which acts much
like [`string`](@ref) but preserves any styling present in the arguments.
"""
struct StyledString{S <: AbstractString} <: AbstractString
    string::S
    properties::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}
end

"""
    StyledChar{S <: AbstractChar} <: AbstractChar

A Char annotated by properties.

More specifically, this is a thin wrapper around any other [`AbstractChar`](@ref),
which adds arbitrary named annotations to the wrapped character.

## Constructors

```julia
StyledChar(s::S) -> StyledChar{S}
StyledChar(s::S, props::Pair{Symbol, <:Any}...)
StyledChar(s::S, properties::Vector{Pair{Symbol, <:Any}})
```
"""
struct StyledChar{C <: AbstractChar} <: AbstractChar
    char::C
    properties::Vector{Pair{Symbol, Any}}
end

# Constructors

StyledString(s::AbstractString, prop::Pair{Symbol, <:Any}, props::Pair{Symbol, <:Any}...) =
    StyledString(s, firstindex(s):lastindex(s), prop, props...)

StyledString(s::AbstractString, region::UnitRange{Int}, props::Pair{Symbol, <:Any}...) =
    StyledString(s, [(region, Pair{Symbol, Any}(first(p), last(p)))
                      for p in props])

StyledString(s::AbstractString, props::Vector{<:Pair{Symbol, <:Any}}) =
    StyledString(s, [(firstindex(s):lastindex(s), p) for p in props])

# Constructors called with overly-specialised arguments

StyledString(s::AbstractString, props::Vector{<:Tuple{UnitRange{Int}, <:Pair{Symbol, <:Any}}}) =
    StyledString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}(props))

StyledChar(c::AbstractChar, prop::Pair{Symbol, <:Any}, props::Pair{Symbol, <:Any}...) =
    StyledChar(c, Vector{Pair{Symbol, Any}}(vcat(prop, props...)))

# Constructors to avoid recursive wrapping

StyledString(s::StyledString, props::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}) =
    StyledString(s.string, vcat(s.properties, props))

StyledChar(c::StyledChar, props::Vector{Pair{Symbol, Any}}) =
    StyledChar(c.char, vcat(s.properties, props))

# Conversion/promotion

Base.convert(::Type{StyledString}, s::StyledString) = s
Base.convert(::Type{StyledString{S}}, s::S) where {S <: AbstractString} =
    StyledString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}())
Base.convert(::Type{StyledString}, s::S) where {S <: AbstractString} =
    convert(StyledString{S}, s)
StyledString(s::S) where {S <: AbstractString} = convert(StyledString{S}, s)

Base.convert(::Type{StyledChar}, c::StyledChar) = c
Base.convert(::Type{StyledChar{C}}, c::C) where { C <: AbstractChar } =
    StyledChar{C}(c, Vector{Pair{Symbol, Any}}())
Base.convert(::Type{StyledChar}, c::C) where { C <: AbstractChar } =
    convert(StyledChar{C}, c)

StyledChar(c::AbstractChar) = convert(StyledChar, c)
StyledChar(c::UInt32) = convert(StyledChar, Char(c))
StyledChar{C}(c::UInt32) where {C <: AbstractChar} = convert(StyledChar, C(c))

Base.promote_rule(::Type{<:StyledString}, ::Type{<:AbstractString}) = StyledString

# AbstractString interface

Base.ncodeunits(s::StyledString) = ncodeunits(s.string)
Base.codeunits(s::StyledString) = codeunits(s.string)
Base.codeunit(s::StyledString) = codeunit(s.string)
Base.codeunit(s::StyledString, i::Integer) = codeunit(s.string, i)
Base.isvalid(s::StyledString, i::Integer) = isvalid(s.string, i)
Base.@propagate_inbounds Base.iterate(s::StyledString, i::Integer=firstindex(s)) =
    if i <= lastindex(s.string); (s[i], nextind(s, i)) end
Base.eltype(::Type{<:StyledString{S}}) where {S} = StyledChar{eltype(S)}
Base.firstindex(s::StyledString) = firstindex(s.string)
Base.lastindex(s::StyledString) = lastindex(s.string)

function Base.getindex(s::StyledString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds if isvalid(s, i)
        StyledChar(s.string[i], textproperties(s, i))
    else Base.string_index_err(s, i) end
end

# AbstractChar interface

Base.ncodeunits(c::StyledChar) = ncodeunits(c.char)
Base.codepoint(c::StyledChar) = codepoint(c.char)

# Avoid the iteration fallback with comparison

Base.cmp(a::StyledString, b::AbstractString) = cmp(a.string, b)
Base.cmp(a::AbstractString, b::StyledString) = cmp(a, b.string)

# Concatenation

(Base.:*)(a::StyledString, b::StyledString) =
    StyledString(a.string * b.string,
                 vcat(a.properties,
                      [(start + lastindex(a) => stop + lastindex(a), prop)
                       for ((start, stop), prop) in b.properties]))

(Base.:*)(a::StyledString, b::AbstractString) = a * convert(StyledString, b)
(Base.:*)(a::AbstractString, b::StyledString) = convert(StyledString, a) * b

"""
    styledstring(xs...)

Create a `StyledString` from any values using the `print` function.

This acts like [`string`](@ref) (which see), but takes care to preserve any
styling information present (in the form of `StyledString` or `StyledChar`
entries).
"""
function styledstring(xs...)
    isempty(xs) && return StyledString("")
    size = mapreduce(Base._str_sizehint, +, xs)
    s = IOContext(IOBuffer(sizehint=size), :color => true)
    properties = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    # TODO merge identical properties with overlapping/adjacent ranges
    for x in xs
        if x isa StyledString
            for (region, prop) in x.properties
                push!(properties, (s.io.size .+ (region), prop))
            end
            print(s, x.string)
        elseif x isa StyledChar
            for prop in x.properties
                push!(properties, (1+s.io.size:1+s.io.size, prop))
            end
            print(s, x.char)
        else
            print(s, x)
        end
    end
    StyledString(String(resize!(s.io.data, s.io.size)), properties)
end

styledstring(s::StyledString) = s
styledstring(c::StyledChar) = StyledString(string(c.char), c.properties)

StyledString(s::SubString{<:StyledString}) = styledstring(s)

function Base.join(iterator, delim::StyledString, last=delim)
    xs = zip(iterator, Iterators.repeated(delim)) |> Iterators.flatten |> collect
    xs = xs[1:end-1]
    if length(xs) > 1
        xs[end-1] = last
    end
    styledstring(xs...)
end

# A copy of `Base.lpad` just swapping out `string` for `styledstring`,
# but also with more meaningful variable/argument names and less type assertions.
function Base.lpad(str::Union{StyledString, StyledChar, SubString{<:StyledString}},
                   width::Integer, pad::Union{AbstractChar,AbstractString}=' ')
    gap = signed(width) - textwidth(str)
    gap ≤ 0 && return styledstring(str)
    width = textwidth(pad)
    quot, rem = divrem(gap, width)
    if rem == 0
        styledstring(pad^quot, str)
    else
        styledstring(pad^quot, first(pad, rem), str)
    end
end

# A copy of `Base.rpad` just swapping out `string` for `styledstring`,
# but also with more meaningful variable/argument names and less type assertions.
function Base.rpad(str::Union{StyledString, StyledChar, SubString{<:StyledString}},
                   width::Integer, pad::Union{AbstractChar,AbstractString}=' ')
    gap = signed(width) - textwidth(str)
    gap ≤ 0 && return styledstring(str)
    width = textwidth(pad)
    quot, rem = divrem(gap, width)
    if rem == 0
        styledstring(str, pad^quot)
    else
        styledstring(str, pad^quot, first(pad, rem))
    end
end

function Base.repeat(str::StyledString, r::Integer)
    r == 0 && return one(StyledString)
    r == 1 && return str
    unstyled = repeat(str.string, r)
    properties = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    len = ncodeunits(str)
    for offset in 0:len:(r-1)*len
        for (region, prop) in str.properties
            push!(properties, (region .+ offset, prop))
        end
    end
    StyledString(unstyled, properties)
end

Base.repeat(str::SubString{StyledString}, r::Integer) =
    repeat(StyledString(str), r)

# TODO optimise?
Base.repeat(c::StyledChar, r::Integer) = repeat(styledstring(c), r)

# Because `RegexMatch` hardcodes `String`.
# If this is PR'd to Julia, it would probably
# be worth adding a string type parameter to
# `RegexMatch`, and maybe see if we can avoid
# this reimplementation of `match` entirely.
struct StyledRegexMatch{S} <: AbstractMatch
    match::SubString{S}
    captures::Vector{Union{Nothing, SubString{S}}}
    offset::Int64
    offsets::Vector{Int64}
    regex::Regex
end

function Base.match(re::Regex, str::StyledString{S}) where {S}
    m = match(re, str.string)
    if !isnothing(m)
        StyledRegexMatch{StyledString{S}}(
            # It's surprisingly annoying to clone a substring,
            # thanks to that pesky inner constructor.
            eval(Expr(:new, SubString{StyledString{S}},
                      str, m.match.offset, m.match.ncodeunits)),
            map(m.captures) do c
                if !isnothing(c)
                    eval(Expr(:new, SubString{StyledString{S}},
                              str, c.offset, c.ncodeunits))
                end
            end,
            m.offset,
            m.offsets,
            re)
    end
end

# TODO implement `Base.replace(::StyledString, ...)`

# End AbstractString interface

"""
    textproperty!(s::StyledString, [range::UnitRange{Int}], prop::Symbol, val)
    textproperty!(s::SubString{StyledString}, [range::UnitRange{Int}], prop::Symbol, val)

Set `prop` to `val` in `s`, over either `range` if specified or the whole string.
"""
function textproperty!(s::StyledString, range::UnitRange{Int}, prop::Symbol, val)
    indices = searchsorted(s.properties, (range,), by=first)
    propindex = filter(i -> first(s.properties[i][2]) === prop, indices)
    if length(propindex) == 1
        if val === nothing
            deleteat!(s.properties, first(propindex))
        else
            s.properties[first(propindex)] = (range, Pair{Symbol, Any}(prop, val))
        end
    else
        splice!(s.properties, indices, [(range, Pair{Symbol, Any}(prop, val))])
    end
    s
end

textproperty!(ss::StyledString, prop::Symbol, value) =
    textproperty!(ss, firstindex(ss):lastindex(ss), prop, value)

textproperty!(s::SubString{<:StyledString}, range::UnitRange{Int}, prop::Symbol, value) =
    (textproperty!(s.string, s.offset .+ (range), prop, value); s)

textproperty!(s::SubString{<:StyledString}, prop::Symbol, value) =
    (textproperty!(s.string, s.offset .+ (1:s.ncodeunits), prop, value); s)

# TODO: optimise
"""
    textproperties(s::StyledString, i::Integer)
    textproperties(s::SubString{StyledString}, i::Integer)

Get the text properties that apply to `s` at index `i`.
"""
function textproperties(s::StyledString, i::Integer)
    props = filter(prop -> !isempty(intersect(i:i, first(prop))),
                   s.properties)
    last.(props)
end

textproperties(s::SubString{<:StyledString}, i::Integer) =
    textproperties(s.string, s.offset + i)

"""
    textproperties(c::StyledChar)

Get the properties that apply to `c`.
"""
textproperties(c::StyledChar) = c.properties

# Iterating over styles

struct StyleIterator{S <: AbstractString}
    str::S
    regions::Vector{UnitRange{Int}}
    styles::Vector{Vector{Pair{Symbol, Any}}}
end

Base.eltype(::StyleIterator{S}) where { S <: AbstractString} =
    Tuple{SubString{S}, Vector{Pair{Symbol, Any}}}

"""
    eachstyle(s::StyledString{S})
    eachstyle(s::SubString{StyledString{S}})

Identify the contiguously substrings of `s` with a constant style, and return
an iterator which provides each substring and the applicable styles as a
`Tuple{SubString{S}, Vector{Pair{Symbol, Any}}}`.

# Examples

```jldoctest
julia> eachstyle(StyledString("hey there", [(1:3, :face => :bold),
                                            (5:9, :face => :italic)])) |> collect
3-element Vector{Tuple{SubString{String}, Vector{Pair{Symbol, Any}}}}:
 ("hey", [:face => :bold])
 (" ", [])
 ("there", [:face => :italic])
```
"""
function eachstyle(s::StyledString, region::UnitRange{Int}=firstindex(s):lastindex(s))
    isempty(s) || isempty(region) &&
        return StyleIterator(s, Vector{UnitRange{Int}}(), Vector{Vector{Pair{Symbol, Any}}}())
    regions = Vector{UnitRange{Int}}()
    styles = Vector{Vector{Pair{Symbol, Any}}}()
    changepoints = filter(c -> c in region,
                          Iterators.flatten((first(region), nextind(s, last(region)))
                                            for region in first.(s.properties)) |>
                                                unique |> sort)
    isempty(changepoints) &&
        return StyleIterator(s.string, [region], [textproperties(s, first(region))])
    function registerchange(start, stop)
        push!(regions, start:stop)
        push!(styles, textproperties(s, start))
    end
    if first(region) < first(changepoints)
        registerchange(first(region), prevind(s, first(changepoints)))
    end
    for (start, stop) in zip(changepoints, changepoints[2:end])
        registerchange(start, prevind(s, stop))
    end
    if last(changepoints) <= last(region)
        registerchange(last(changepoints), last(region))
    end
    StyleIterator(s.string, regions, styles)
end

eachstyle(s::SubString{<:StyledString}) = if isempty(s)
    StyleIterator(s, Vector{UnitRange{Int}}(), Vector{Vector{Pair{Symbol, Any}}}())
else
    eachstyle(s.string, 1+s.offset:1+s.offset+s.ncodeunits-ncodeunits(last(s)))
end

Base.length(si::StyleIterator) = length(si.regions)

Base.@propagate_inbounds Base.iterate(si::StyleIterator, i::Integer=1) =
    if i <= length(si.regions)
        @inbounds ((si.str[si.regions[i]], si.styles[i]), i+1)
    end
