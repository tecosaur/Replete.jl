function Base.show(io::IO, @nospecialize(x))
    if get(io, :color, false) == false
        Base.show_default(io, x)
    else
        out = IOContext(IOBuffer(),
                        merge(IOContext(io).dict,
                              pairs((; color=false)))...)
        Base.show_default(out, x)
        print(io, highlight(out))
    end
end
