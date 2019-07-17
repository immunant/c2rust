DEBUG = false

function debug(str)
    if DEBUG then
        print(str)
    end
end

function starts_with(str, start)
    return str:sub(1, #start) == start
end
