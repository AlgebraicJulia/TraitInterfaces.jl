module Algorithms 

export sortcheck, rename

using MLStyle

using ..Interfaces, ..Syntax
using ..InterfaceData: add_judgment!, add_alias!

"""
Throw an error if a the head of an AlgTerm (which refers to a term constructor)
has arguments of the wrong sort. Returns the sort of the term.
"""
function sortcheck(theory::Interface, ctx::TypeScope, t::AlgTerm)::AlgSort
  if t isa TermApp 
    argsorts = sortcheck.(Ref(theory), Ref(ctx), t.args)
    m = t.method
    judgment = lookup(theory, get(theory.aliases, m, m), argsorts)
    judgment isa TermConstructor || error("Bad judgment $t \n$judgment")
    AlgSort(judgment.type)
  elseif t isa TermVar 
    AlgSort(ctx[get(t)])
  end
end

#######################
# Renaming interfaces #
#######################

"""
This is boilerplate for the systematic renaming of types/operations in an 
interface.
"""
function rename(theory::Interface, names::Dict{Symbol, Symbol})
  I = Interface(theory.name)
  for j in theory.judgments
    new_j = rename(j, names)
    add_judgment!(I, new_j)
  end
  for (k, v) in theory.aliases
    add_alias!(I, get(names, k, k), get(names, v, v))
  end
  I
end

function rename(j::AlgAxiom, names::Dict{Symbol, Symbol})
  AlgAxiom(j.name, rename(j.localcontext, names), rename(j.sort, names),
           rename.(j.equands, Ref(names)))
end

function rename(j::TypeConstructor, names::Dict{Symbol, Symbol})
  TypeConstructor(get(names, j.name, j.name), rename(j.localcontext, names), j.args, rename.(j.typeargs, Ref(names)))
end

function rename(j::TermConstructor, names::Dict{Symbol, Symbol})
  TermConstructor(get(names, j.name, j.name), rename(j.localcontext, names), j.args, rename(j.type, names))
end

function rename(j::AlgSort, names::Dict{Symbol, Symbol})
  AlgSort(get(names, j.method, j.method), j.vararg)
end

rename(j::TermVar, ::Dict{Symbol, Symbol}) = j

function rename(j::TermApp, names::Dict{Symbol, Symbol}) 
  TermApp(get(names, j.method, j.method), rename.(j.args, Ref(names)))
end

function rename(j::TypeApp, names::Dict{Symbol, Symbol})
  TypeApp(get(names, j.method, j.method), rename.(j.args, Ref(names)),
          rename.(j.params, Ref(names)))
end

rename(j::VarArgType, names::Dict{Symbol, Symbol}) = 
  VarArgType(rename(get(j), names))

rename(s::Symbol, names::Dict{Symbol,Symbol}) = get(names, s, s)

function rename(j::TypeScope, names::Dict{Symbol, Symbol})
  TypeScope([n => rename(a, names) for (n,a) in j.args], 
            [n => rename(a, names) for (n,a) in j.kwargs])
end

end # module
