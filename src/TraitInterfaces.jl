module TraitInterfaces

using Reexport 

include("MetaUtils.jl")

include("Interfaces/module.jl")
@reexport using .Interfaces

include("Implementations/module.jl")
@reexport using .Implementations


end # module TraitInterfaces
