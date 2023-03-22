using REPL # for `istruecolor()`

"""
    istruecolor()

Return a boolean signifying whether the current terminal supports 24-bit colors.

This uses the `COLORTERM` environment variable if possible, returning true if it
is set to either `"truecolor"` or `"24bit"`.

As a fallback, first on unix systems the `colors` terminal capability is checked
with `tput` if possible --- should more than 256 colors be reported, this is
taken to signify 24-bit support. Lastly, the color is attempted to be set to
#010203 and then the current color queried via the DCS sequence `\$qm`.  If the
output contains `":1:2:3"` this is taken to signify 24-bit support.

If the fallbacks are used, the `"COLORTERM"` entry in `ENV` is updated according
to the result. This ensures that frequent calls will only potentially be slow
the first time.
"""
function istruecolor()
    function test24bitcolorDCS()
        REPL.Terminals.raw!(REPL.TerminalMenus.terminal, true)
        print(stdout, "\e[48;2;1;2;3m\eP\$qm\e\\\e[m")
        flush(stdout)
        # Some terminals are bad and haven't got DCS sequence support,
        # if we don't see a response from stdin we need to abort.
        output = @task readuntil(stdin, 'm')
        schedule(output)
        Timer(0.1) do _
            istaskdone(output) || Base.throwto(output, InterruptException())
        end
        color = try
            fetch(output)
        catch _ "" end
        REPL.Terminals.raw!(REPL.TerminalMenus.terminal, false)
        occursin(":1:2:3", color)
    end
    if haskey(ENV, "COLORTERM")
        ENV["COLORTERM"] in ("truecolor", "24bit")
    elseif Sys.isunix() && !isnothing(Sys.which("tput")) &&
        parse(Int, read(`tput colors`, String)) > 256 ||
        isinteractive() && test24bitcolorDCS()
        # Since we may have just gone to a whole bunch of effort,
        # let's save the result so we don't have to do this again.
        ENV["COLORTERM"] = "truecolor"
        true
    else
        ENV["COLORTERM"] = "no"
        false
    end
end

"""
A mapping between ANSI named colours and indices in the standard 256-color
table. The standard colors are 0-7, and high intensity colors 8-15.

The high intensity colors are prefixed by "light_". The "light_black" color is
given two aliases: "grey" and "gray".
"""
const ANSI_4BIT_COLORS = Dict{Symbol, Int}(
    :black => 0,
    :red => 1,
    :green => 2,
    :yellow => 3,
    :blue => 4,
    :magenta => 5,
    :cyan => 6,
    :white => 7,
    :light_black => 8,
    :grey => 8,
    :gray => 8,
    :light_red => 9,
    :light_green => 10,
    :light_yellow => 11,
    :light_blue => 12,
    :light_magenta => 13,
    :light_cyan => 14,
    :light_white => 15)

"""
    ansi_4bit_color_code(color::Color{Symbol}, background::Bool=false)

Provide the color code (30-37, 40-47, 90-97, 100-107) for `color`, as a string.
When `background` is set the background variant will be provided, otherwise
the provided code is for setting the foreground color.
"""
function ansi_4bit_color_code(color::Color{Symbol}, background::Bool=false)
    if haskey(ANSI_4BIT_COLORS, color.value)
        code = ANSI_4BIT_COLORS[color.value]
        code >= 8 && (code += 52)
        background && (code += 10)
        string(code + 30)
    else
        ifelse(background, "49", "39")
    end
end

"""
    termcolor8bit(io::IO, color::Color{RGBTuple}, category::Char)

Print to `io` the best 8-bit SGR color code that sets the `category` color to
be close to `color`.
"""
function termcolor8bit(io::IO, color::Color{RGBTuple}, category::Char)
    # Magic numbers? Lots.
    (; r, g, b) = color.value
    cdistsq(r1, g1, b1) = (r1 - r)^2 + (g1 - g)^2 + (b1 - b)^2
    to6cube(value) = if value < 48; 0
    elseif value < 114; 1
    else (value - 35) ÷ 40 end
    r6cube, g6cube, b6cube = to6cube(r), to6cube(g), to6cube(b)
    sixcube = (0, 95, 135, 175, 215, 255)
    rnear, gnear, bnear = sixcube[r6cube], sixcube[g6cube], sixcube[b6cube]
    colorcode = if r == rnear && g == gnear && b == bnear
        16 + 35 * r6cube + 6 * g6cube + b6cube
    else
        grey_avg = (r + g + b) ÷ 3
        grey_index = if grey_avg > 238 23 else (grey_avg - 3) ÷ 10 end
        grey = 8 + 10 * grey_index
        if cdistsq(grey, grey, grey) <= cdistsq(rnear, gnear, bnear)
            232 + grey
        else
            16 + 35 * r6cube + 6 * g6cube + b6cube
        end
    end
    print(io, "\e[", category, "8;5;", string(colorcode), 'm')
end

"""
    termcolor24bit(io::IO, color::Color{RGBTuple}, category::Char)

Print to `io` the 24-bit SGR color code to set the `category`8 slot to `color`.
"""
function termcolor24bit(io::IO, color::Color{RGBTuple}, category::Char)
    print(io, "\e[", category, "8;2;",
          string(color.value.r), ';',
          string(color.value.g), ';',
          string(color.value.b), 'm')
end

"""
    termcolor(io::IO, color::Color, category::Char)

Print to `io` the SGR code to set the `category`'s slot to `color`,
where `category` is set as follows:
- `'3'` sets the foreground color
- `'4'` sets the background color
- `'5'` sets the underline color

If `color` is a `Color{Symbol}`, the value should be a a member of
`ANSI_4BIT_COLORS`. Any other value will cause the color to be reset.

If `color` is a `Color{RGBTuple}` and `istruecolor()` returns true, 24-bit color
is used. Otherwise, an 8-bit approximation of `color` is used.
"""
termcolor(io::IO, color::Color{RGBTuple}, category::Char) =
    if istruecolor()
        termcolor24bit(io, color, category)
    else
        termcolor8bit(io, color, category)
    end

termcolor(io::IO, color::Color{Symbol}, category::Char) =
    print(io, "\e[",
          if category == '3' || category == '4'
              ansi_4bit_color_code(color, category == '4')
          elseif category == '5'
              if haskey(ANSI_4BIT_COLORS, color)
                  string("58;5;", ANSI_4BIT_COLORS[color])
              else "59" end
          end,
          'm')

"""
    termcolor(io::IO, ::Nothing, category::Char)

Print to `io` the SGR code to reset the color for `category`.
"""
termcolor(io::IO, ::Nothing, category::Char) =
    print(io, "\e[", category, '9', 'm')

function termstyle(io::IO, face::Face, lastface::Face=FACES[:default])
    face.foreground == lastface.foreground ||
        termcolor(io, face.foreground, '3')
    face.background == lastface.background ||
        termcolor(io, face.background, '4')
    face.weight == lastface.weight ||
        print(io, if face.weight in (:semibold, :bold, :extrabold, :ultrabold)
            "\e[1m"
        elseif face.weight in (:semilight, :light, :extralight, :ultralight)
            "\e[2m"
        else # :normal
            "\e[22m"
        end)
    face.slant == lastface.slant ||
        print(io, ifelse(face.slant in (:italic, :oblique), "\e[3m", "\e[23m"))
    face.underline == lastface.underline || # TODO fancy kitty underline support
        print(io, ifelse(face.underline === true, "\e[4m", "\e[24m"))
    # Kitty fancy underlines, see <https://sw.kovidgoyal.net/kitty/underlines>
    # Supported in Kitty, VTE, iTerm2, Alacritty, and Wezterm.
    face.underline == lastface.underline ||
        if face.underline isa Tuple # Color and style
            color, style = face.underline
            print(io, "\e[4:", if style == :straight; '1'
                      elseif style == :double; '2'
                      elseif style == :curly; '3'
                      elseif style == :dotted; '4'
                      elseif style == :dashed; '5'
                      else '0' end,
                  "0m")
            !isnothing(color) && termcolor(io, color, '5')
        elseif face.underline isa Color{RGBTuple}
            if !(lastface.underline isa Color{RGBTuple} || lastface.underline == true)
                print(io, "\e[4m")
            end
            termcolor(io, face.underline, '5')
        elseif face.underline == true
            print(io, "\e[4m")
        else
            print(io, "\e[24m")
        end
    face.strikethrough == lastface.strikethrough ||
        print(io, ifelse(face.strikethrough === true, "\e[9m", "\e[29m"))
    face.inverse == lastface.inverse ||
        print(io, ifelse(face.inverse === true, "\e[7m", "\e[27m"))
end

function Base.print(io::IO, s::Union{<:StyledString, SubString{<:StyledString}})
    if get(io, :color, false) == true
        lastface = FACES[:default]
        for (str, styles) in eachstyle(s)
            face = getface(styles)
            link = let idx=findfirst(==(:link) ∘ first, styles)
                if !isnothing(idx) last(styles[idx]) end end
            !isnothing(link) && print(io, "\e]8;;", link, "\e\\")
            termstyle(io, face, lastface)
            print(io, str)
            !isnothing(link) && print(io, "\e]8;;\e\\")
            lastface = face
        end
        termstyle(io, FACES[:default], lastface)
    elseif s isa StyledString
        print(io, s.string)
    elseif s isa SubString
        print(io, eval(Expr(:new, SubString{typeof(s.string.string)},
                            s.string.string, s.offset, s.ncodeunits)))
    end
end

Base.show(io::IO, ::MIME"text/plain", s::Union{<:StyledString, SubString{<:StyledString}}) =
    print(io, '\"', s, '"')

function Base.print(io::IO, c::StyledChar)
    if get(io, :color, false) == true
        termstyle(io, getface(c), FACES[:default])
        print(io, c.char)
        termstyle(io, FACES[:default], getface(c))
    else
        print(io, c.char)
    end
end

function Base.show(io::IO, ::MIME"text/plain", c::StyledChar)
    if get(io, :color, false) == true
        out = IOBuffer()
        invoke(show, Tuple{IO, AbstractChar}, out, c)
        print(io, ''', StyledString(String(take!(out)[2:end-1]), c.properties), ''')
    else
        show(io, c.char)
    end
end
