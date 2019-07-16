DEBUG = false

function debug(str)
    if DEBUG then
        print(str)
    end
end

function starts_with(str, start)
    return str:sub(1, #start) == start
end

Set = {}

function Set.new(items)
    self = {}

    for _, i in ipairs(items) do self[i] = true end

    setmetatable(self, Set)
    Set.__index = Set

    return self
end
