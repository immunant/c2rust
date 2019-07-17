-- Take a set of node ids (params) and turn them (if a pointer) into a reference
Visitor = {}

function Visitor.new(node_id)
   self = {}
   self.node_ids = node_ids

   setmetatable(self, Visitor)
   Visitor.__index = Visitor

   return self
end

function Visitor:visit_arg(arg)
    arg_id = arg:get_id()

    if self.node_ids[arg_id] then
        arg_ty = arg:get_ty()

        if arg_ty:get_kind() == "Ptr" then
            mut_ty = arg_ty:get_mut_ty()

            if self.node_ids[arg_id] == "ref_slice" then
                pointee_ty = mut_ty:get_ty()
                pointee_ty:wrap_in_slice()
                mut_ty:set_ty(pointee_ty)
            end

            arg_ty:to_rptr(nil, mut_ty)
            arg:set_ty(arg_ty)
        end
    end
end

refactor:transform(
    function(transform_ctx)
        node_ids = {
            [12] = "ref",
            [21] = "ref",
            [58] = "ref_slice",
        }
        return transform_ctx:visit_crate_new(Visitor.new(node_ids))
    end
)

print("Finished upgrade_ptr_to_ref.lua")
