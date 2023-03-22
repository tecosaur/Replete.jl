"""
    StyledStrings

Add arbitrary properties to strings and characters. This library adds two types
that wrap any `AbstractString` and `AbstractChar` and allow for regions within
the string to be annotated with properties. See the `StyledString` and
`StyledChar` docstrings for more information on those types.

A set of functions for working with styled content are also provided. For
setting and querying properties there is `textproperty!` and `textproperties`.
For combining styled and unstyled content, `styledstring` acts as a
style-preserving analogue of `string`.

To make formatted text easier to work with, a `Face` type is provided which
bundles together several graphical attributes. There is a set of global named
faces that can be extended with `addface!`, modified with `loadfaces!`, and
reset with `resetfaces!`. The set of global faces can be temporarily modified via
the `withfaces` function. Fancy printing support has been implemented for
terminal output.

Faces can easily be applied to text via the `face!` function, or with the
`S"..."` string macro.
"""
module StyledStrings

include("styledstrings.jl")
include("faces.jl")
include("stringfaces.jl")

include("termstyling.jl")

export StyledString, StyledChar, textproperty!, textproperties,
    eachstyle, styledstring, Face, face!, addface!, loadfaces!, resetfaces!,
    withfaces, @S_str

function __init__()
    userfaces = joinpath(first(DEPOT_PATH), "config", "Faces.toml")
    isfile(userfaces) && loadfaces!(userfaces)
end

end
