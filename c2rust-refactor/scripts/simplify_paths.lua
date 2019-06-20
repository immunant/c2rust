--- Simplify absolute paths to relative paths and add `use` declarations
local inspect = require "inspect" 

local Visitor = {}

function Visitor.new(transform_ctx)
   self = {}
   self.cur_path = {}
   self.ctx = transform_ctx

   setmetatable(self, Visitor)
   Visitor.__index = Visitor

   return self
end

function Visitor:flat_map_item(item, walk)
   if item:get_kind() == "Mod" then
      table.insert(self.cur_path, item:get_ident())
      walk(item)
      table.remove(self.cur_path)
   else
      self.ctx:visit_paths(
         item,
         function(id, qself, path, def)
            -- Ignore paths with <..>
            if path:has_generic_args() then
               return qself, path
            end

            local segments = path:get_segments()
            local same_count = 0
            for i,segment in ipairs(segments) do
               if ((i == 1 and segment == "crate" and self.cur_path[i] == "") or
                   segment == self.cur_path[i]) then
                  same_count = same_count + 1
               else
                  break
               end
            end
            if same_count > 1 then
               while same_count > 0 do
                  table.remove(segments, 1)
                  same_count = same_count - 1
               end
            end
            path:set_segments(segments)
            return qself, path
         end
      )
   end

   return {item}
end


refactor:transform(
   function(transform_ctx)
      transform_ctx:visit_crate_new(Visitor.new(transform_ctx))
   end
)
