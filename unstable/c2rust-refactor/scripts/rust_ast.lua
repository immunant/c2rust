Ty = {}

function Ty.new(kind)
    local self = {}

    self[1] = "Ty"
    self.id = DUMMY_NODE_ID
    self.span = DUMMY_SP
    self.kind = kind

    setmetatable(self, Ty)
    Ty.__index = Ty

    return self
end

Expr = {}

function Expr.new(kind, attrs)
    local self = {}

    self[1] = "Expr"
    self.id = DUMMY_NODE_ID
    self.span = DUMMY_SP
    self.kind = kind
    self.attrs = attrs or {}

    setmetatable(self, Expr)
    Expr.__index = Expr

    return self
end

Pat = {}

function Pat.new(kind)
    local self = {}

    self[1] = "Pat"
    self.id = DUMMY_NODE_ID
    self.span = DUMMY_SP
    self.kind = kind

    setmetatable(self, Pat)
    Pat.__index = Pat

    return self
end

Local = {}

function Local.new(pat, ty, init, attrs)
    local self = {}

    self[1] = "Local"
    self.id = DUMMY_NODE_ID
    self.span = DUMMY_SP
    self.pat = pat
    self.ty = ty
    self.init = init
    self.attrs = attrs

    return self
end

Stmt = {}

function Stmt.new(kind)
    local self = {}

    self[1] = "Stmt"
    self.kind = kind

    return self
end

Ident = {}

function Ident.new(name)
    local self = {}

    self[1] = "Ident"
    self.name = name
    self.span = DUMMY_SP

    setmetatable(self, Ident)
    Ident.__index = Ident

    return self
end

PathSegment = {}

-- Ident is assumed to be a string, but we could check the type
-- and skip Ident.new() if it is an Ident already
function PathSegment.new(ident, args)
    local self = {}

    self[1] = "PathSegment"
    self.ident = Ident.new(ident)
    self.args = args
    self.id = DUMMY_NODE_ID

    setmetatable(self, PathSegment)
    PathSegment.__index = PathSegment

    return self
end

Path = {}

-- Segments are assumed to be strings, but we could check the type
-- and skip PathSegment.new() if it is a PathSegment already
function Path.new(segments)
    local self = {}
    local path_segments = {}

    for _, segment in ipairs(segments) do
        table.insert(path_segments, PathSegment.new(segment))
    end

    self[1] = "Path"
    self.span = DUMMY_SP
    self.segments = path_segments

    setmetatable(self, Path)
    Path.__index = Path

    return self
end
