using TOML # For `loadfaces!`

const RGBTuple = NamedTuple{(:r, :g, :b), Tuple{UInt8, UInt8, UInt8}}

"""
A basic representation of a color, intended for string styling purposes.
It can either contain a named color (like `:red`), or an `RGBTuple` which
is a NamedTuple specifying an `r`, `g`, `b` color with a bit-depth of 8.

## Constructors

```julia
Color(name::Symbol) -> Color{Symbol}     # e.g. :red
Color(rgb::RGBTuple) -> Color{RGBTuple}  # e.g. (r=1, b=2, g=3)
Color(r::Integer, b::Integer, b::Integer) -> Color{RGBTuple}
Color(rgb::UInt32) -> Color{RGBTuple}    # e.g. 0x123456
```

Also see `tryparse(Color, rgb::String)`.
"""
struct Color{C <: Union{Symbol, RGBTuple}}
    value::C
end

Color(r::Integer, g::Integer, b::Integer) = Color((; r=UInt8(r), g=UInt8(g), b=UInt8(b)))
Color(rgb::UInt32) = Color(reverse(reinterpret(UInt8, [rgb]))[2:end]...)

"""
    tryparse(::Type{Color}, rgb::String)

Attempt to parse `rgb` as a `Color`. If `rgb` starts with
`#` and has a length of 7, it is converted into a `Color{RGBTuple}`.
If `rgb` starts with `a`-`z`, `rgb` is interpreted as a color name
and converted to a `Color{Symbol}`.

Otherwise, `nothing` is returned.
"""
function Base.tryparse(::Type{Color}, rgb::String)
    if startswith(rgb, '#') && length(rgb) == 7
        Color(parse(Int, rgb[2:3], base=16),
              parse(Int, rgb[4:5], base=16),
              parse(Int, rgb[6:7], base=16))
    elseif startswith(rgb, 'a':'z')
        Color(Symbol(rgb))
    end
end

"""
    parse(::Type{Color}, rgb::String)

An analogue of `tryparse(Color, rgb::String)` (which see),
that raises an error instead of returning `nothing`.
"""
Base.parse(::Type{Color}, rgb::String) =
    @something(tryparse(Color, rgb),
               throw(ArgumentError("invalid color \"$rgb\"")))

"""
A [`Face`](@ref) is a collection of graphical attributes for displaying text.
Faces control how text is displayed in the terminal, and possibly other
places too.

Most of the time, a [`Face`](@ref) will be stored in the [`FACES`](@ref) dict as
a unique association with a *face name* Symbol, and will be most often reffered
to by this name instead of the `Face` object itself.

## Attributes

All attributes can be set via the keyword constructor, and default to `nothing`.

- `foreground` (a `Color`): The text foreground color.
- `background` (a `Color`): The text backgrond color.
- `weight` (a `Symbol`): One of the symbols (from faintest to densest)
  `:ultralight`, `:extralight`, `:light`, `:semilight`, `:normal`,
  `:semibold`, `:bold`, `:extrabold`, or `:ultrabold`.
  In terminals any weight greater than `:normal` is displayed as bold,
  and in terminals that support variable-brightness text, any weight
  less than `:normal` is displayed as faint.
- `slant` (a `Symbol`): One of the symbols `:italic`, `:oblique`, or `:normal`.
- `underline`, the text underline, which takes one of the following forms:
  - a `Bool`: Whether the text should be underlined or not.\\
  - a `Color`: The text should be underlined with this color.\\
  - a `Tuple{Nothing, Symbol}`: The text should be underlined using the style
    set by the Symbol, one of `:straight`, `:double`, `:curly`, `:dotted`,
    or `:dashed`.\\
  - a `Tuple{Color, Symbol}`: The text should be underlined in the specifed
    Color, and using the style specified by the Symbol, as before.
- `strikethrough` (a `Bool`): Whether the text should be struck through.
- `inverse` (a `Bool`): Whether the foreground and background colors should be
  inverted.
- `inherit` (a `Tuple{Vararg{Symbol}}`): Names of faces to inherit from,
  with earlier faces taking priority. All faces inherit from the `:default` face.
"""
struct Face
    font::Union{Nothing, String}
    foreground::Union{Nothing, Color}
    background::Union{Nothing, Color}
    weight::Union{Nothing, Symbol}
    slant::Union{Nothing, Symbol}
    underline::Union{Nothing, Bool, Color, Tuple{Union{Nothing, Color}, Symbol}}
    strikethrough::Union{Nothing, Bool}
    inverse::Union{Nothing, Bool}
    inherit::Union{Nothing, Tuple{Vararg{Symbol}}}
end

Face(; font::Union{Nothing, String} = nothing,
     foreground::Union{Nothing, Color} = nothing,
     background::Union{Nothing, Color} = nothing,
     weight::Union{Nothing, Symbol} = nothing,
     slant::Union{Nothing, Symbol} = nothing,
     underline::Union{Nothing, Bool, Color, Tuple{Color, Symbol}} = nothing,
     strikethrough::Union{Nothing, Bool, Color} = nothing,
     inverse::Union{Nothing, Bool} = nothing,
     inherit::Union{Nothing, Tuple{Vararg{Symbol}}} = nothing,
     _...) = # Simply ignore unrecognised keyword arguments.
         Face(font, foreground, background, weight, slant,
              underline, strikethrough, inverse, inherit)

Base.convert(::Type{Face}, spec::NamedTuple) = Face(; spec...)

Face(specs::Pair{Symbol, <:Any}...) =
    convert(Face, NamedTuple{first.(specs)}(last.(specs)))

# For underline styles, see https://sw.kovidgoyal.net/kitty/underlines/

"""
The default values for all defined faces. [`FACES`](@ref) is initially set to a
copy of this value. This essentially allows for any modifications to
[`FACES`](@ref) to be undone.
"""
const DEFAULT_FACES = Dict{Symbol, Face}(
    # Default is special, everything inherits from it
    :default => Face(font = "monospace",
                     foreground=Color(:default),
                     background=Color(:default),
                     weight=:normal, slant=:normal,
                     underline=false, strikethrough=false,
                     inverse=false),
    # Property faces
    :bold => Face(weight=:bold),
    :italic => Face(slant=:italic),
    :underline => Face(underline=true),
    :strikethrough => Face(strikethrough=true),
    :inverse => Face(inverse=true),
    # Useful common faces
    :shadow => Face(foreground=Color(:grey)),
    :region => Face(background=Color(0x3a3a3a)),
    :error => Face(foreground=Color(:red)),
    :warning => Face(foreground=Color(:yellow)),
    :success => Face(foreground=Color(:green)),
    :emphasis => Face(foreground=Color(:blue)),
    :highlight => Face(inherit=(:emphasis,), inverse=true),
    # Julia prompts
    :julia_prompt => Face(weight=:bold),
    # REPL faces
    :repl_history_search_prompt => Face(foreground=Color(:blue),
                                        inherit=(:julia_prompt,)),
    :repl_history_search_seperator => Face(foreground=Color(:blue)),
    :repl_history_search_prefix => Face(foreground=Color(:magenta)),
    :repl_history_search_selected => Face(foreground=Color(:blue)),
    :repl_history_search_unselected => Face(foreground=Color(:grey)),
    :repl_history_search_preview_box => Face(foreground=Color(:grey)),
    :repl_history_search_results => Face(inherit=(:shadow,)),
    # Julia syntax highlighting faces
    :julia_identifier => Face(foreground=Color(:light_white)),
    :julia_macro => Face(foreground=Color(:magenta)),
    :julia_symbol => Face(foreground=Color(:magenta)),
    :julia_nothing => Face(inherit=(:julia_symbol,)),
    :julia_type => Face(foreground=Color(:yellow)),
    :julia_comment => Face(foreground=Color(:grey)),
    :julia_string => Face(foreground=Color(:green)),
    :julia_string_delim => Face(foreground=Color(:light_green)),
    :julia_cmdstring => Face(inherit=(:julia_string,)),
    :julia_char => Face(inherit=(:julia_string,)),
    :julia_char_delim => Face(inherit=(:julia_string_delim,)),
    :julia_number => Face(foreground=Color(:light_red)),
    :julia_bool => Face(foreground=Color(:light_red)),
    :julia_funcall => Face(foreground=Color(:cyan)),
    :julia_operator => Face(foreground=Color(:cyan)),
    :julia_comparison => Face(foreground=Color(:yellow)),
    :julia_assignment => Face(foreground=Color(:light_blue)),
    :julia_keyword => Face(foreground=Color(:red)),
    :julia_error => Face(background=Color(:red)),
    # Rainbow delimitors (1-6, (), [], and {})
    :julia_parenthetical => Face(),
    :julia_rainbow_paren_1 => Face(foreground=Color(:light_green)),
    :julia_rainbow_paren_2 => Face(foreground=Color(:light_blue)),
    :julia_rainbow_paren_3 => Face(foreground=Color(:light_red)),
    :julia_rainbow_paren_4 => Face(inherit=(:julia_rainbow_paren_1,)),
    :julia_rainbow_paren_5 => Face(inherit=(:julia_rainbow_paren_2,)),
    :julia_rainbow_paren_6 => Face(inherit=(:julia_rainbow_paren_3,)),
    :julia_rainbow_bracket_1 => Face(foreground=Color(:blue)),
    :julia_rainbow_bracket_2 => Face(foreground=Color(:light_magenta)),
    :julia_rainbow_bracket_3 => Face(inherit=(:julia_rainbow_bracket_1,)),
    :julia_rainbow_bracket_4 => Face(inherit=(:julia_rainbow_bracket_2,)),
    :julia_rainbow_bracket_5 => Face(inherit=(:julia_rainbow_bracket_1,)),
    :julia_rainbow_bracket_6 => Face(inherit=(:julia_rainbow_bracket_2,)),
    :julia_rainbow_curly_1 => Face(foreground=Color(:light_yellow)),
    :julia_rainbow_curly_2 => Face(foreground=Color(:yellow)),
    :julia_rainbow_curly_3 => Face(inherit=(:julia_rainbow_curly_1,)),
    :julia_rainbow_curly_4 => Face(inherit=(:julia_rainbow_curly_2,)),
    :julia_rainbow_curly_5 => Face(inherit=(:julia_rainbow_curly_1,)),
    :julia_rainbow_curly_6 => Face(inherit=(:julia_rainbow_curly_2,)),
)

"""
The global set of availible faces.
"""
FACES::Dict{Symbol, Face} = copy(DEFAULT_FACES)

# Adding and resetting faces

"""
    addface!(name::Symbol, default::Face)

Create a new face by the name `name`. So long as no face already exists by this
name, `default` is added to both [`DEFAULT_FACES`](@ref) and (a copy of) to
[`FACES`](@ref), with the [`FACES`](@ref) value returned.

Should the face `name` already exist, `nothing` is returned.
"""
function addface!(name::Symbol, default::Face)
    if !haskey(DEFAULT_FACES, name)
        DEFAULT_FACES[name] = default
        FACES[name] = copy(default)
    end
end

"""
    resetfaces!()

Reset [`FACES`](@ref) to the default value (`DEFAULT_FACES`).
"""
function resetfaces!()
    global FACES = copy(DEFAULT_FACES)
end

"""
    resetfaces!(name::Symbol)

Reset the face `name` to its default value, which is returned.
If the face `name` does not exist, nothing is done and `nothing` returned.
In the unlikely event that the face `name` does not have a default value,
it is deleted, a warning message is printed, and `nothing` returned.
"""
function resetfaces!(name::Symbol)
    if !haskey(FACES, name)
    elseif haskey(DEFAULT_FACES, name)
        FACES[name] = copy(DEFAULT_FACES[name])
    else # This shouldn't happen
        delete!(FACES, name)
        @warn """The face $name was reset, but it had no default value, and so has been deleted instead!",
                 This should not have happened, perhaps the face was added without using `addface!`?"""
    end
end

"""
    withfaces(f, kv::Pair...)

Execute `f` with [`FACES`](@ref) temporarily modified by zero or more `:name =>
val` arguments `kv`. `withfaces` is generally used via the `withfaces(kv...) do
... end` syntax. A value of `nothing` can be used to temporarily unset an
environment variable (if it is set). When `withfaces` returns, the original
[`FACES`](@ref) has been restored.

    !!! warning
    Changing faces is not thread-safe.
"""
function withfaces(f, keyvals::Pair{Symbol, Union{Face, Symbol, Nothing}}...)
    old = Dict{Symbol, Union{Face, Nothing}}()
    for (name, face) in keyvals
        old[name] = get(FACES, face, nothing)
        if face isa Face
            FACES[name] = face
        elseif face isa Symbol
            FACES[name] = get(FACES, face, Face())
        elseif haskey(FACES, name)
            delete!(FACES, name)
        end
    end
    try f()
    finally
        for (name, face) in old
            if isnothing(face)
                delete!(FACES, name)
            else
                FACES[name] = face
            end
        end
    end
end

"""
    withfaces(f, altfaces::Dict{Symbol, Face})

Execute `f` with [`FACES`](@ref) temporarily swapped out with `ALTFACES`
When `withfaces` returns, the original [`FACES`](@ref) has been restored.

    !!! warning
    Changing faces is not thread-safe.
"""
function withfaces(f, altfaces::Dict{Symbol, Face})
    global FACES
    oldfaces, FACES = FACES, altfaces
    try f()
    finally
        FACES = oldfaces
    end
end

withfaces(f) = f()

# Face combination and inheritence

"""
    merge(a::Face, b::Face)
Merge the properties of the text faces `a` and `b`, with those
of `b` taking priority.
"""
Base.merge(a::Face, b::Face) = if isnothing(a.inherit)
    Face(ifelse(isnothing(b.font),          a.font,          b.font),
         ifelse(isnothing(b.foreground),    a.foreground,    b.foreground),
         ifelse(isnothing(b.background),    a.background,    b.background),
         ifelse(isnothing(b.weight),        a.weight,        b.weight),
         ifelse(isnothing(b.slant),         a.slant,         b.slant),
         ifelse(isnothing(b.underline),     a.underline,     b.underline),
         ifelse(isnothing(b.strikethrough), a.strikethrough, b.strikethrough),
         ifelse(isnothing(b.inverse),       a.inverse,       b.inverse),
         b.inherit)
else
    merge(merge(a), b)
end

"""
    merge(faces::Face...)
    merge(faces::Vector{Face})

Merge the properties of `faces` into a single face, with the properties
of subsequent faces overriding the properties of prior faces.
"""
Base.merge(a::Face, b::Face, others::Face...) = merge(merge(a, b), others...)
Base.merge(faces::Vector{Face}) = foldl(merge, faces, init=Face())

"""
    merge(face::Face)

Merge any inherited faces of `face` into a self-contained [`Face`](@ref).
"""
function Base.merge(face::Face)
    if isnothing(face.inherit)
        merge(FACES[:default], face)
    else
        noinheritface = Face(
            face.font, face.foreground, face.background, face.weight, face.slant,
            face.underline, face.strikethrough, face.inverse, nothing)
        inherited = mapfoldl(fname -> get(FACES, fname, Face()), merge,
                            Iterators.flatten(((:default,),
                                                Iterators.reverse(face.inherit))))
        merge(merge(inherited), noinheritface)
    end
end

# Getting the combined face from a set of properties

"""
    getface(faces)

Obtain the final merged face from `faces`, an iterator of
[`Face`](@ref)s, face name `Symbol`s, and lists thereof.
"""
function getface(faces)
    expandface(face::Face) = face
    expandface(face::Symbol) = get(FACES, face, Face())
    expandfaces(faces::Vector) = expandface.(faces)
    expandfaces(faces) = (expandface(faces),)
    face = foldl(merge,
                 Iterators.map(expandfaces, faces) |> Iterators.flatten,
                 init=Face()) |> merge
end

"""
    getface(styles::Vector{Pair{Symbol, Any}})

Combine all of the `:face` styles with `getfaces`.
"""
getface(styles::Vector{Pair{Symbol, Any}}) =
    getface(Iterators.map(last, Iterators.filter(
        prop -> first(prop) === :face, styles)))

# Reading face definitions from TOML

"""
    loadfaces!((name, update)::Pair{Symbol, Face})

Merge the face `name` in [`FACES`](@ref) with `update`. If the face `name` does
not already exist in [`FACES`](@ref), then it is set to `update.`
"""
loadfaces!((name, update)::Pair{Symbol, Face}) =
    if haskey(FACES, name)
        FACES[name] = merge(FACES[name], update)
    else
        FACES[name] = update
    end

"""
    loadfaces!(faces::Dict{String, Any})

For each face specified in `Dict`, load it to [`FACES`](@ref).
"""
loadfaces!(faces::Dict{String, Any}) =
    for (name, spec) in faces
        loadfaces!(Symbol(name) => convert(Face, spec))
    end

"""
    loadfaces!(facetoml::IO)

Parse `facetoml` as TOML, and load all faces described.
The loading is done with `loadfaces!`, which see.

Face entries should be of the following form:
```toml
[face_name]
property = "value"
"""
loadfaces!(facetoml::IO) = loadfaces!(TOML.parse(facetoml))

"""
    loadfaces!(facetoml::String)

Open `facetoml`, parse it as TOML, and load all faces described.
The loading is done with `loadfaces!`, which see.
"""
loadfaces!(facetoml::String) =
    open(facetoml, "r") do io loadfaces!(io) end

function Base.convert(::Type{Face}, spec::Dict)
    Face(if haskey(spec, "font") && spec["font"] isa String
             tryparse(Color, spec["font"]) end,
        if haskey(spec, "foreground") && spec["foreground"] isa String
             tryparse(Color, spec["foreground"]) end,
         if haskey(spec, "background") && spec["background"] isa String
             tryparse(Color, spec["background"]) end,
         if haskey(spec, "weight") && spec["weight"] isa String
             Symbol(spec["weight"])
         elseif haskey(spec, "bold") && spec["bold"] isa Bool
             ifelse(spec["bold"], :bold, :normal)
         end,
         if haskey(spec, "slant") && spec["slant"] isa String
             Symbol(spec["slant"])
         elseif haskey(spec, "italic") && spec["italic"] isa Bool
             ifelse(spec["italic"], :italic, :normal)
         end,
         if !haskey(spec, "underline")
         elseif spec["underline"] isa Bool
             spec["underline"]
         elseif spec["underline"] isa String
             tryparse(Color, spec["underline"])
         elseif spec["underline"] isa Vector && length(spec["underline"]) == 2
             color == tryparse(Color, spec["underline"][1])
             (color, Symbol(spec["underline"][2]))
         end,
         if !haskey(spec, "strikethrough")
         elseif spec["strikethrough"] isa Bool
             spec["strikethrough"]
         elseif spec["strikethrough"] isa String
             tryparse(Color, spec["strikethrough"])
         end,
         if haskey(spec, "inverse") && spec["inverse"] isa Bool
             spec["inverse"] end,
         if !haskey(spec, "inherit")
         elseif spec["inherit"] isa String
             [Symbol(spec["inherit"])]
         elseif spec["inherit"] isa Vector{String}
             Symbol.(spec["inherit"])
         end)
end
