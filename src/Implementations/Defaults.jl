module Defaults 
export @default_model

using MLStyle

using ...MetaUtils: Expr0
using ..ModelInterface, ...Interfaces

"""
Create an @instance for a model `M` whose methods are determined by type 
dispatch, e.g.:

```
@instance ThCategory{O,H} [model::M] begin
  id(x::O) = id(x)
  compose(f::H, g::H)::H = compose(f, g)
end
```

Use this with caution! For example, using this with two different models of 
the same theory with the same types would cause a conflict.
"""
function default_instance(theorymodule, theory, jltype_by_sort, model)
  acc = Iterators.flatten(values.(values(theory.accessors)))

  termcon_funs = map(last.(termcons(theory)) ∪ acc) do x 
    generate_function(use_dispatch_method_impl(x, theory, jltype_by_sort))
  end
  generate_instance(
    theory, theorymodule, jltype_by_sort, model, [], 
    Expr(:block, termcon_funs...); escape=true)
end

"""
Create an @instance for a model `M` whose methods are determined by the 
implementation of another model, `M2`, e.g.:

```
@instance ThCategory{O,H} [model::M] begin
  id(x::O) = id[M2](x)
  compose(f::H, g::H)::H = compose[M2](f, g)
end
```
"""
function default_model(theorymodule, theory, jltype_by_sort, model)
  acc = Iterators.flatten(values.(values(theory.accessors)))
  termcon_funs = map(last.(termcons(theory)) ∪ acc) do x 
    generate_function(use_model_method_impl(x, theory, jltype_by_sort, model))
  end
  generate_instance(
    theory, theorymodule, jltype_by_sort, nothing, [], 
    Expr(:block, termcon_funs...);  escape=true)
end

macro default_model(head, model)
  # Parse the head of @instance to get theory and instance types
  (theory_module, instance_types) = @match head begin
    :($ThX{$(Ts...)}) => (ThX, Ts)
    _ => error("invalid syntax for head of @instance macro: $head")
  end

  # Get the underlying theory
  theory = macroexpand(__module__, :($theory_module.Meta.@theory))

  # A dictionary to look up the Julia type of a type constructor from its name (an ident)
  jltype_by_sort = Dict{AlgSort,Expr0}([
    zip(primitive_sorts(theory), instance_types)..., 
    [s => nameof(headof(s)) for s in struct_sorts(theory)]...
  ]) 

  # Get the model type that we are overloading for, or nothing if this is the
  # default instance for `instance_types`
  m = parse_model_param(model)[1]

  # Create the actual instance
  default_model(theory_module, theory, jltype_by_sort, m)
end

# changed x from Ident to Symbol
"""
A canonical implementation that just calls the method with the implementation
of another model, `m`.
"""
function use_model_method_impl(x::Symbol, theory::Interface, 
                               jltype_by_sort::Dict{AlgSort}, m::Expr0)
  op = getvalue(theory[x])
  name = nameof(getdecl(op))
  return_type = op isa AlgAccessor ? nothing : jltype_by_sort[AlgSort(op.type)]
  args = args_from_sorts(sortsignature(op), jltype_by_sort)
  impl = :(return $(name)[$m()]($(args...)))
  JuliaFunction(name=name, args=args, return_type=return_type, impl=impl)
end

# changed x from Ident to Symbol
"""
A canonical implementation that just calls the method with type dispatch.
"""
function use_dispatch_method_impl(x::Symbol, theory::Interface, 
                                  jltype_by_sort::Dict{AlgSort})
  op = getvalue(theory[x])
  name = nameof(getdecl(op))
  return_type = op isa AlgAccessor ? nothing : jltype_by_sort[AlgSort(op.type)]
  args = args_from_sorts(sortsignature(op), jltype_by_sort)
  impl = :(return $(name)($(args...)))
  JuliaFunction(name=name, args=args, return_type=return_type, impl=impl)
end


end # module
