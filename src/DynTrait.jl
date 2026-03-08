using MLStyle: @λ
using DataStructures: OrderedDict

const LNN = LineNumberNode

function makefunc(name::Symbol, fieldtoparam::OrderedDict, fieldtotheory::OrderedDict, fieldtogensym::OrderedDict, fieldtotype::OrderedDict)
    implementcalls = [Expr(:macrocall, Symbol("@assert"), LNN, Expr(:call, :implements, field, type)) for (field, type) in fieldtotheory]
    bindings = [Expr(:(::), field, type) for (field, type) in fieldtotype]
    params = unique(values(fieldtoparam)) 
    # TODO ^ we need to make sure they are in the same order as the typeparams...
    # TODO ^ why unique?
    Expr(:function,
        Expr(:where, Expr(:call, name, bindings...), params...),
        Expr(:block, implementcalls...,
             # for every field whose type implements an interface,
             Expr(:call, Expr(:curly, :new, params...), keys(fieldtotype)...)))
end

""" @impl

This macro interprets `⊢` (\vdash) bindings in a struct as bindings to fields which implement a certain interface specified by @instance.
```julia
@impl Graph
    V⊢ThRing
    E⊢ThRing
end
```
Here is an example of a struct with trait-type, a concrete type, and a type parameter.
```julia
@impl _J{S}
    a⊢ThMonoid
    b::Int
    c::S
end
```

"""
macro impl(body)
    funcname = nothing 
    # XXX recall that a dictionary is not ordered
    fieldtotype = OrderedDict{Symbol, Symbol}() # e.g., :a => Symbol("##ThRing#999"), :b => :Int, :c => T
    fieldtogensym = OrderedDict{Symbol, Symbol}() # e.g., :a => Symbol("##ThRing#999")
    fieldtoparam = OrderedDict{Symbol, Symbol}() # fieldtotype, but restricted to parameters
    fieldtotheory = OrderedDict{Symbol, Symbol}() # e.g., :a => :ThRing
    ordinaryparams = nothing
    expr = @λ begin
        # struct A{T}; ... end
        Expr(:struct, ismutable, Expr(:curly, name, typeparams...), e) => begin
            funcname = name
            ordinaryparams = typeparams
            block = expr(e)
            theoryparams = values(fieldtoparam)
            Expr(:struct, ismutable, Expr(:curly, name, theoryparams...), block)
        end
        # struct A; ... end
        Expr(:struct, ismutable, name, e) => begin
            funcname = name
            block = expr(e) 
            theoryparams = values(fieldtoparam)
            Expr(:struct, ismutable, Expr(:curly, name, theoryparams...), block)
        end
        # a ⊢ ThRing
        Expr(:call, :⊢, field, theory) => begin
            type=gensym(theory) # e.g., Symbol("##ThRing#999")
            push!(fieldtotype, field => type)
            push!(fieldtoparam, field => type)
            push!(fieldtogensym, field => type)
            push!(fieldtotheory, field => theory)
            Expr(:(::), field, type)
        end
        # a::Int
        Expr(:(::), field, type) => begin
            push!(fieldtotype, field => type)
            if !isnothing(ordinaryparams) && type ∈ ordinaryparams
                push!(fieldtoparam, field => type)
            end
            Expr(:(::), field, type)
        end
        #
        Expr(:block, args...) => begin
            args = expr.(args)
            newfun = makefunc(funcname, fieldtoparam, fieldtotheory, fieldtogensym, fieldtotype)
            Expr(:block, args..., newfun)
        end
        #
        s => s
    end
    newbody = expr(body)
    newbody
end
