module TraitInterfaces

using Reexport 

include("MetaUtils.jl")
include("Interfaces/module.jl")
include("Implementations/module.jl")


@reexport using .Interfaces

@reexport using .Implementations


end # module TraitInterfaces
