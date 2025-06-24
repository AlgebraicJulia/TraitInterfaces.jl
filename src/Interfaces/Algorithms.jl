module Algorithms 

export sortcheck

using ..Interfaces, ..Syntax

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

end # module
