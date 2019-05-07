DEBUG = false

function debug(str)
    if DEBUG then
        print(str)
    end
end

Visitor = {}

function Visitor.new()
    self = {}
    self.value = "Obj val"
    self.found_fn_items = {}

    setmetatable(self, Visitor)
    Visitor.__index = Visitor

    return self
end

function Visitor:visit_item(item)
    debug(item.kind)
    if item.kind == "Fn" then
        debug("Found fn item:")
        table.insert(self.found_fn_items, item)
    end

    return true
end

function Visitor:finish()
    debug("Finishing: " .. self.value)

    debug("Renaming:")
    for i, item in ipairs(self.found_fn_items) do
        debug("Renamed " .. item.ident .. " to renamed" .. i)
        item.ident = "renamed" .. i
    end
end

refactor:transform(
    function(transform_ctx, crate)
        return transform_ctx:run_visitor(Visitor.new(), crate)
    end
)

print("Finished example_visitor.lua")
