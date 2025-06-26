module Algorithms 

export sortcheck, rename

using ..Interfaces, ..Syntax
using ..Interfaces: add_judgment!

"""
Throw an error if a the head of an AlgTerm (which refers to a term constructor)
has arguments of the wrong sort. Returns the sort of the term.
"""
function sortcheck(theory::Interface, ctx::TypeScope, t::AlgTerm)::AlgSort
  if t.body isa MethodApp
    argsorts = sortcheck.(Ref(theory), Ref(ctx), t.body.args)
    m = t.body.method
    judgment = lookup(theory, get(theory.aliases, m, m), argsorts)
    judgment isa TermConstructor || error("Bad judgment $t \n$judgment")
    AlgSort(judgment.type)
  else
    AlgSort(ctx[t.body])
  end
end

function rename(theory::Interface, names::Dict{Symbol, Symbol})
  I = Interface(theory.name)
  for j in theory.judgments
    new_j = rename(j, names)
    add_judgment!(I, new_j)
  end
  for (k, v) in theory.aliases
    Interfaces.add_alias!(I, get(names, k, k), get(names, v, v))
  end
  I
end

function rename(j::AlgAxiom, names::Dict{Symbol, Symbol})
  AlgAxiom(j.name, rename(j.localcontext, names), rename(j.sort, names),
           rename.(j.equands, Ref(names)))
end

function rename(j::TypeConstructor, names::Dict{Symbol, Symbol})
  TypeConstructor(get(names, j.name, j.name), rename(j.localcontext, names), j.args)
end

function rename(j::TermConstructor, names::Dict{Symbol, Symbol})
  TermConstructor(get(names, j.name, j.name), rename(j.localcontext, names), j.args, rename(j.type, names))
end

function rename(j::AlgSort, names::Dict{Symbol, Symbol})
  AlgSort(get(names, j.method, j.method))
end

function rename(j::T, names::Dict{Symbol, Symbol}) where {T<:Union{AlgTerm, AlgType}}
  T(rename(j.body, names))
end

rename(s::Symbol, names::Dict{Symbol,Symbol}) = get(names, s, s)

function rename(j::MethodApp{T}, names::Dict{Symbol, Symbol}) where T
  MethodApp{T}(get(names, j.method, j.method), rename.(j.args, Ref(names)))
end


function rename(j::TypeScope, names::Dict{Symbol, Symbol})
  TypeScope([n => rename(a, names) for (n,a) in j.args], 
            [n => rename(a, names) for (n,a) in j.kwargs])
end


end # module
