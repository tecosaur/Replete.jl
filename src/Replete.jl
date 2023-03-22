module Replete

using REPL
using JuliaSyntax
using TOML

include("StyledStrings/StyledStrings.jl")

using .StyledStrings

include("selector.jl")
include("highlight.jl")

function __init__()
    # Make sure the enclosed code only runs once
    if ccall(:jl_generating_output, Cint, ()) == 0
        include(joinpath(@__DIR__, "overrides.jl"))
    end
end

end
